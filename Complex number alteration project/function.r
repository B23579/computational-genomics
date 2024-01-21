
library(tidyverse)
as_ploidy = function(copy_state){
  copy_state %>% nrow()
}

# generate a new mutation ID
new_mutation = function(copy_state){
  used = copy_state %>% get_mutations()
  id = NULL
  repeat{
    id = sample(LETTERS, 8, replace = TRUE) %>% paste(collapse = "")
    if(!id %in% used ) break
  }
  
  return(id)
}
# Get stast
as_karyotype = function(copy_state){
  tab_counts = copy_state %>% get_alleles() %>% substr(0, 1) %>% table() %>% sort(decreasing = TRUE)
  state = as.numeric(tab_counts) %>% paste(collapse = ':')
  if(!grepl(":", state)) state = paste0(state, ':0')
  
  return(state)
}

# getters
get_mutations = function(copy_state, allele = NULL){
  if(is.null(allele))
    mutations = copy_state %>% pull(mutations) %>% unlist()
  else
    mutations = copy_state %>% filter(allele == !!allele) %>% pull(mutations) %>% unlist()
  
  return(mutations)
}

# Add mutations to all alleles
mutation = function(copy_state){
  for(i in 1:nrow(copy_state))
  {
    muts = copy_state$mutations[[i]] %>% unlist()
    muts = list(c(muts, copy_state %>% new_mutation))
    copy_state$mutations[[i]] = muts
  }
  copy_state
}

# initial state
initial_state = function(target)
{
  copy_state = data.frame(allele = c("A1", "B1")) %>% as_tibble()
  copy_state$mutations = NULL
  copy_state$mutations[[1]] = list()
  copy_state$mutations[[2]] = list()
  
  copy_state = copy_state %>% mutation()
  
  if(target == "1:1") return(copy_state)
  else evolve(copy_state, target)[[1]]
} 
l= initial_state("1:1")
l_g=get_mutations(l)

get_alleles = function(copy_state){
  copy_state$allele
}
get_alleles(l)
####
# Delete allele
delete =  function(copy_state){
  cancel = function(which_allele)
  {
    mutations_allele = copy_state %>% get_mutations(allele = which_allele)
    
    copy_state %>% filter(allele != which_allele) %>% arrange(allele)
  }
  
  copy_state %>%
    get_alleles() %>%
    lapply(cancel)
}

# genome_double
genome_double =  function(copy_state){
  copy_of = copy_state
  
  for(i in 1:nrow(copy_of))
  {
    ni_allele = substr(copy_state$allele[i], 2, nchar(copy_state$allele[i])) %>% as.numeric
    ci_allele = substr(copy_state$allele[i], 0, 1)
    
    # New allele can be +1, unless there are other alleles with larger number
    new_t = paste0(ci_allele, ni_allele + 1)
    
    while(new_t %in% (copy_state %>% bind_rows(copy_of) %>%  get_alleles()) %>% unique) {
      ni_allele = ni_allele + 1
      new_t = paste0(ci_allele, ni_allele + 1)
    }
    
    copy_of$allele[i] = new_t
    # copy_of$allele[i] = paste0(ci_allele, ni_allele + 1)
  }
  
  copy_state %>% bind_rows(copy_of) %>% arrange(allele) %>%  list()
}

# Multiplicity
multiplicities = function(copy_state){
  copy_state %>% get_mutations() %>% table()
}

# Get stast
as_karyotype = function(copy_state){
  tab_counts = copy_state %>% get_alleles() %>% substr(0, 1) %>% table() %>% sort(decreasing = TRUE)
  state = as.numeric(tab_counts) %>% paste(collapse = ':')
  if(!grepl(":", state)) state = paste0(state, ':0')
  
  return(state)
}

# Amplify allele
amplify =  function(copy_state){
  augment = function(which_allele)
  {
    mutations_allele = copy_state %>% get_mutations(allele = which_allele)
    
    maj_min = substr(which_allele, 0, 1)
    n_allele = gsub(x = which_allele, 'A', "") %>% gsub(pattern = 'B', replacement = "") %>%
      as.numeric()
    
    # New allele can be +1, unless there are other alleles with larger number
    new_allele = paste(maj_min, n_allele + 1, sep = '')
    
    while(new_allele %in% (copy_state %>% get_alleles())) {
      n_allele = n_allele + 1
      new_allele = paste(maj_min, n_allele + 1, sep = '')
    }
    
    new_entry = copy_state %>% filter(allele == which_allele)
    new_entry$allele = new_allele
    
    copy_state %>% bind_rows(new_entry) %>% dplyr::arrange(allele)
  }
  
  copy_state %>%
    get_alleles() %>%
    lapply(augment)
}

# Delete allele
delete =  function(copy_state){
  cancel = function(which_allele)
  {
    mutations_allele = copy_state %>% get_mutations(allele = which_allele)
    
    copy_state %>% filter(allele != which_allele) %>% arrange(allele)
  }
  
  copy_state %>%
    get_alleles() %>%
    lapply(cancel)
}

# Evolution models
evolve = function(copy_state, target)
{
  if(copy_state %>% as_karyotype() == target) return(copy_state %>% mutation() %>% list())
  
  # ploidy of the target copy state
  cap = (target %>% strsplit(split = ':'))[[1]] %>% as.numeric %>% sum
  cap = 2 * cap
  
  current_state = original_state = list(copy_state)
  
  repeat{
    amp_new_state = lapply(current_state, amplify) %>% unlist(recursive = FALSE)
    del_new_state = lapply(current_state, delete) %>% unlist(recursive = FALSE)
    wgs_new_state = lapply(current_state, genome_double) %>% unlist(recursive = FALSE)
    
    # Filter by capping
    amp_new_state = amp_new_state[sapply(amp_new_state, as_ploidy) <= cap]
    del_new_state = del_new_state[sapply(del_new_state, as_ploidy) <= cap]
    wgs_new_state = wgs_new_state[sapply(wgs_new_state, as_ploidy) <= cap]
    
    current_state = amp_new_state %>%
      append(del_new_state) %>%
      append(wgs_new_state)
    
    what_we_have = current_state %>% sapply(as_karyotype)
    
    # print(what_we_have)
    
    if(target %in% what_we_have) {
      target_state = current_state[which(target == what_we_have)]
      break;
    }
  }
  
  # Remove duplicates - based on allele identities
  identities = sapply(target_state, function(x) x$allele %>% sort() %>% paste(collapse = ''))
  target_state = target_state[which(!duplicated(identities))]
  
  return(target_state %>% lapply(mutation))
}
####
join_strings <- function(strings) {
  return(paste(strings, collapse = ''))
}

get_peaks = function(clone_1, CCF_1, purity)
{
  c(
    clone_1 %>% get_mutations()
  ) %>%
    table()
  
  m_c1 = clone_1 %>% get_mutations() %>% table() %>% as_tibble() %>%
    mutate(x = n * CCF_1,
           karyotype_1 = clone_1 %>% as_karyotype(),
           genotype_1 = clone_1 %>% get_alleles() %>% sort() %>% paste(collapse = '')
    )
  colnames(m_c1)[1] = 'mutation'
  
  denominator = 2 * (1-purity) +
    purity * ( CCF_1 * (clone_1 %>% as_ploidy()))
  
  m_c1 %>%
    tidyr::replace_na(list(n = 0, x = 0)) %>%
    dplyr::mutate(peak = (x) * purity) %>%
    dplyr::distinct(peak, .keep_all = TRUE) %>%
    dplyr::mutate(peak = peak/denominator) %>%
    dplyr::select(mutation, karyotype_1, genotype_1, n, peak) %>%
    dplyr::arrange(peak)
}