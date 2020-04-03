# Hansard cleaner script created by Matt Chaib (original found at: mattchaib.com)
# Modified by Paulino Diaz

# FUNCTIONS ---------------------------------------------------------------

create_header_table <- function(hansard_plaintext) {
  # pull out all headers
  tibble(headers = unlist(str_extract_all(hansard_plaintext, "\n[^\n\r]*\r"))) %>%
    # mark those short enough to be a name
    mutate(possible_name = str_length(headers) < 150 & !str_detect(headers, "\r\n\r\n")) %>% 
    filter(possible_name)
}

condense_header_table <- function(header_table) {
  header_table %>% 
    # chop off escape characters
    mutate(headers = str_sub(headers, start = 2, end = -2),
           # pull out first word of header
           first_word = str_sub(str_extract(headers, "[a-zA-Z]*\\W"), 1, -2), 
           # pull out first two words of header
           first_two_words = str_sub(str_extract(headers, "[a-zA-Z]*\\W[a-zA-Z'-]*"), 1, -1)) %>% 
    group_by(first_word) %>% 
    nest() %>%
    mutate(n_obs = map_int(data, nrow), 
           first_two_word = map(data, "first_two_words"), 
           entire_title = map(data, "headers")) 
}

peers_from_condensed_table <- function(c_ht) {
  mps <- read_csv(here("mps.csv")) %>%
    unite(col = "name", c("First name", "Last name"), sep = " ") %>%
    select(name)
  titles <- c("Lord", "The Lord", "The Advocate", "The Advocate-General",
              "The Earl", "Earl", "The Countess","Countess","Archbishop",
              "The Archbishop","The Parliamentary","Viscount","Baroness",
              "The Baroness", mps$name)
  
  peers_identified <- c_ht %>%
    select(-data, -n_obs) %>%
    unnest() %>%
    filter(first_word %in% titles | first_two_word %in% titles) 
}

mark_peer_metadata <- function(peers_id) {
  peers_id %>%
    mutate(contains_party = str_count(entire_title, "\\(")>0,
           awkward = str_count(entire_title, "\\(")==2)
}

extract_base_names <- function(peers_id) {
  peers_id %>% 
    mutate(name = case_when(
      # if two sets of brackets, get name before brackets
      awkward ~ str_sub(str_extract(entire_title, "[a-zA-Z ',\\-]+"), 1, -2), 
      # if party, ignore party
      !awkward & contains_party ~ str_sub(str_extract(entire_title, "[a-zA-Z ',\\-]+"), 1, -2), 
      # if just a name, just use the name
      !awkward & !contains_party ~ entire_title 
    ))
}

create_parties_from_wiki <- function(peers) {
  printf <- function(...) invisible(print(sprintf(...)))
  
  load_party_data <- function() {
    lords <- read_csv(here("peers.csv")) 
    mps <- read_csv(here("mps.csv")) %>%
      unite(col = "name", c("First name", "Last name"), sep = " ")
    lords %<>% select(Name, Party) %>% rename(name = Name, party = Party)
    mps %<>% select(name, Party) %>% rename(party = Party)
    wiki_ids <- bind_rows(lords, mps)  
  }
  wiki_ids <- load_party_data()
  peers_w_party <- peers %>% left_join(wiki_ids, by = "name")
  return(peers_w_party)
}

check_party <- function(peers_id) {
  printf <- function(...) invisible(print(sprintf(...)))

  n_unassigned <- sum(is.na(peers_id$party))
  
  if (n_unassigned == 0) {
    printf("All Peers assigned to a party!")
  } else {
    printf("%i Peers could not be assigned to a party. Check is.na(.$party).", n_unassigned)
  }
}

bind_text <- function(hansard_plaintext, peers, remove_exclamations=T) {
  
  # First task, find all headers and their start and end locations
  loc <- tibble(headers = str_sub(unlist(str_extract_all(hansard_plaintext, "\n[^\n\r]*\r")), start = 2, end = -2),
                start = str_locate_all(hansard_plaintext, "\n[^\n\r]*\r")[[1]][,1],
                end = str_locate_all(hansard_plaintext, "\n[^\n\r]*\r")[[1]][,2]) 
  
  # extract the headers which we have previously confirmed as belonging to peers
  peer_headers <- unique(peers$entire_title) # all headers corresponding to a peer
  
  # Using the headers assigned to peers and their locations in the text,
  # assume that all the text between these headers belongs to a single speech for that peer,
  # and assign it to that Lord.
  peers <- distinct(peers)
  
  text <- loc %>%
    filter(headers %in% peer_headers) %>%
    mutate(speech_start = end, speech_end = lead(start),
           speech_end = ifelse(is.na(speech_end), str_length(hansard_plaintext), speech_end)) %>% # If last speech, it ends at the end of the hansard (bit sketchy)
    select(-start, -end) %>%
    mutate(text = str_sub(hansard_plaintext, speech_start, speech_end),
           text = str_replace_all(text, "\\n|\\r", ""),
           text = str_replace_all(text, "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", "")) %>%
    select(-speech_start, -speech_end) 
  
  # Now clean up this table using the peers tibble
  
  text <- text %>%
    rename(entire_title=headers) %>%
    left_join(peers, by = "entire_title") %>%
    select(name, text, party) %>%
    {if(remove_exclamations) filter(., !str_detect(text, "^My Lords—$")) else .} %>%
    mutate(speech_id = row_number())
  
  return(text)
}

get_hansard_key <- function(hansard_filepath) {
  tibble(hansard_key = str_sub(read_file(hansard_filepath), 1, 200),
         hansard_start_time = str_extract(hansard_key, "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"),
         hansard_date= str_extract(hansard_filepath, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
         hansard_datetime = map2_chr(hansard_date, hansard_start_time, ~str_c(.x, " ", .y)),
         hansard_datetime2 = as.POSIXct(hansard_datetime,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
  )
}

# Process Hansard

process_hansard <- function(hansard_plaintext) {
  
  print(1)
  header_table <- create_header_table(hansard_plaintext)
  
  print(2)
  c_ht <- condense_header_table(header_table)
  
  print(3)
  peers_identified <- peers_from_condensed_table(c_ht)
  
  print(4)
  peers_identified <- mark_peer_metadata(peers_identified)
  
  print(5)
  peers <- extract_base_names(peers_identified)
  
  print(6)
  peers <- create_parties_from_wiki(peers)
  
  print(7)
  check_party(peers)
  
  print(8)
  speeches <- bind_text(hansard_plaintext, peers)
  
  print("Processing Completed")
  
  return(speeches)
}

read_hansards <- function(hansard_filepaths, include_key_as_metadata=F, bind_metadata=F) {
  printf <- function(...) invisible(print(sprintf(...)))
  
  outlist <- list()
  
  for (i in seq_along(hansard_filepaths)) {
    print(i) 
    
    h <- read_file(hansard_filepaths[[i]])
    
    outlist[[i]] <- process_hansard(h)
    
    printf("!!!! Hansard %i completed !!!!", i)
  }
  
  if(include_key_as_metadata & !bind_metadata) {
    meta_list <- list()
    for(i in seq_along(hansard_filepaths)) {
      
      meta_list[[i]] <- get_hansard_key(hansard_filepaths[[i]])
    }
    print("Metadata added!")
    return(list(outlist, meta_list))
  }
  
  if(include_key_as_metadata & bind_metadata) {
    meta_list <- list()
    for(i in seq_along(hansard_filepaths)) {
      
      meta_list[[i]] <- get_hansard_key(hansard_filepaths[[i]])
    }
    print("Metadata added!")
    out <-list(outlist, meta_list)
    
    out <- map2(outlist, meta_list, cbind)
    return(out)
  }
  
  outlist
}
