from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
import sys 


def main():
    gauth = GoogleAuth()           
    drive = GoogleDrive(gauth)  
    file = sys.argv[1]
    display_name = file.split('/')[-1]

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

    drive = GoogleDrive(gauth)


    gfile = drive.CreateFile({
        'title': display_name,
        'parents': [{'id': "1D5YhX9iQE4pW1TS7u_TuNVPAlQj5dxef"}]
        })

    
    gfile.SetContentFile(file)
    gfile.Upload() # Upload the file.

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