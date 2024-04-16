import paramiko

def execute_command(hostname, username, password, command):
    client = paramiko.SSHClient()
    client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
    client.connect(hostname, username=username, password=None)

    stdin, stdout, stderr = client.exec_command(command)
    response = stdout.read().decode('utf-8')
    
    client.close()
    
    return response
