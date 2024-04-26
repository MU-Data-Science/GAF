import subprocess

# SSH command and server details

def run_commands(site,command):
    ssh_command = 'ssh'
    host = 'clnode014.clemson.cloudlab.us'
    host = '2001:400:a100:3010:f816:3eff:fe56:2f87'
    username = 'ubuntu'
    #password = 'your_password'  # Alternatively, you can use key-based authentication

    # Command to create the folder on the remote server
    folder_name = 'test_folder'
    command = f'mkdir {folder_name}'

    # Assemble the SSH command with the necessary options
    # ssh_args = [ssh_command, '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',
    #             f'{username}@{host}', command]

    ssh_args = [ssh_command, '-F','/Users/khawar/.ssh/fabric_ssh_config','-i','/Users/khawar/.ssh/sliver',f'{username}@{host}', command]

    # Execute the SSH command
    try:
        completed_process = subprocess.run(ssh_args, capture_output=True, text=True, check=True)
        output = completed_process.stdout
    except subprocess.CalledProcessError as e:
        output = e.stderr

    # Print the command output
    print(output)