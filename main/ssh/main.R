library(ssh)

session <- ssh_connect("pi@192.168.88.126")

ssh_exec_wait(
  session, 
  command = c(
    "vpns"
    # "neofetch"
  )
)

ssh_disconnect(session)
