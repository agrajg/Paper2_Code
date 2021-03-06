# Check on the nodes we have access to.
node_list = Sys.getenv("SLURM_NODELIST")
cat("SLURM nodes:", node_list, "\n")

# Loop up IPs of the allocated nodes.
if (node_list != "") {
  nds = gsub(x = gsub(pattern = "n\\[", x = node_list, replacement = ""), pattern = "\\]", replacement = "")
  nodes = strsplit(nds, ",")[[1]]
  nodes = paste0("n", nodes)
  ips = rep(NA, length(nodes))
  for (i in 1:length(nodes)) {
    args = c("hosts", nodes[i])
    result = system2("getent", args = args, stdout = T)
    # Extract the IP from the result output.
    ips[i] = sub("^([^ ]+) +.*$", "\\1", result, perl = T)
  }
  cat("SLURM IPs:", paste(ips, collapse=", "), "\n")
  # Combine into a network string for h2o.
  network = paste0(paste0(ips, "/32"), collapse=",")
  cat("Network:", network, "\n")
}

# Specify how many nodes we want h2o to use.
h2o_num_nodes = length(ips)

# Options to pass to java call:
args = c(
  # -Xmx30g allocate 30GB of RAM per node. Needs to come before "-jar"
  # "-Xmx30g",
  "-Xmx90g",
  # Specify path to downloaded h2o jar.
  "-jar /usr/lusers/agrajg/installs/h2o/java/h2o.jar",
  # Specify a cloud name for the cluster.
  "-name h2o_r",
  # Specify 
  # Specify IPs of other nodes.
  paste("-network", network)
)
cat(paste0("Args:\n", paste(args, collapse="\n"), "\n"))

# Run once for each node we want to start.
for (node_i in 1:h2o_num_nodes) {
  cat("\nLaunching h2o worker on", ips[node_i], "\n")
  new_args = c(ips[node_i], "java", args)
  # Ssh into the target IP and launch an h2o worker with its own
  # output and error files. These could go in a subdirectory.
  cmd_result = system2("ssh", args = new_args,
                       stdout = paste0("h2o_out_", node_i, ".txt"),
                       stderr = paste0("h2o_err_", node_i, ".txt"),
                       # Need to specify wait=F so that it runs in the background.
                       wait = F)
  # This should be 0.
  cat("Cmd result:", cmd_result, "\n")
  # Wait one second between inits.
  Sys.sleep(1L)
}

# Wait 3 more seconds to find all the nodes, otherwise we may only
# find the node on localhost.
Sys.sleep(3L)

# Check if h2o is running. We will see ssh processes and one java process.
system2("ps", c("-ef", "| grep h2o.jar"), stdout = T)

# suppressMessages(library(h2oEnsemble))
library(h2o)

# Connect to our existing h2o cluster.
# Do not try to start a new server from R.
h2o.init(startH2O = F)

#################################

source(file = "60_00_DML_Main.R")

#################################
h2o.shutdown(prompt = F)

