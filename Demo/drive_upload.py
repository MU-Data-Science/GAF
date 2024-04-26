from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
import sys 
import os 


def main():
    gauth = GoogleAuth()           
    drive = GoogleDrive(gauth)  
    file_name = sys.argv[1]

    print("inside drive uploading code...")
    gauth.LoadCredentialsFile("mycreds.txt")
    if gauth.credentials is None:
        # Authenticate if they're not there
        gauth.LocalWebserverAuth()
    elif gauth.access_token_expired:
        # Refresh them if expired
        gauth.Refresh()
    else:
        # Initialize the saved creds
        gauth.Authorize()
    # Save the current credentials to a file
    
    
    gauth.SaveCredentialsFile("mycreds.txt")
    print("working till here")

    drive = GoogleDrive(gauth)


    gfile = drive.CreateFile({'parents': [{'id': "1nK1hnETtzVl8gJuM5EdGXvh9ZWo84zgI"}]})

    
    if os.path.exists(file_name):
        print("File exists")
        gfile.SetContentFile(file_name)
        gfile.Upload() # Upload the file.
    else:
        print("file not found")
    
    
   
    file_id = gfile['id']
    # print("File ID of the uploaded file:", file_id)


    # Set the permission for anyone with the link to download the file
    gfile.InsertPermission({
        'type': 'anyone',
        'value': 'anyone',
        'role': 'reader'
    })

    # Get the shareable link to download the file
    shareable_link = gfile['alternateLink']
    print(shareable_link)
    
    
if __name__ == "__main__":
    main()