install.packages("usethis")    # Unless you already have the usethis pacakge
install.packages("gitcreds")   # Unless you already have the gitcreds pacakge
usethis::create_github_token() # This opens up GitHub in your browser to set up
#  a PAT token. Copy and paste the next line into
#  R and then complete the setup on GitHub
#  (stick with defaults), copying the resulting
#  PAT token to your clipboard.
gitcreds::gitcreds_set()       # Paste the PAT token from the clipboard as
#  requested.

#Start 2.4.11