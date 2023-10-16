detach("package:ERGtools2", unload = TRUE)
install.packages("/home/moritz/Dokumente/R/ERGtools2",repos=NULL)
library(ERGtools2)
Sys.unsetenv("GITHUB_PAT")
remotes::install_github("moritzlindner/ERGtools2")
credentials::set_github_pat("YourPAT")
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
