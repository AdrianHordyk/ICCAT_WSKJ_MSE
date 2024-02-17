
# Adrian Hordyk
# February 16 2024

# Script installs latest CRAN of openMSE and latest Github Version of SWOMSE

# Required because all packages have been changed recently

install.packages(c('MSEtool', 'SAMtool'))

remotes::install_github('blue-matter/openMSE')

remotes::install_github('ICCAT/nswo-mse')

packageVersion('SWOMSE') # 0.25.0

