const selections = {
        selection: "Choose Pipeline",
        options:[
            {text:"Germline variant calling",value:'gvc'},
            {text:"Somatic variant calling",value:'svc'},
            // {text:"Joint variant calling",value:'jvc'},
        ]
}

const clusters = {
    selection: "Choose Cluster",
    options:[
        {text:"Cluster 1",value:'cl1'},
        {text:"Cluster 2",value:'cl2'},
        {text:"Cluster 3",value:'cl3'},
        // {text:"Cluster 4",value:'cl4'},
    ]
}

const selectionHeading1 = document.getElementById("selection1");
const selectionOptions1 = document.getElementById("selection-options1");
const selectionHeading2 = document.getElementById("selection2");
const selectionOptions2 = document.getElementById("selection-options2");
const selectionHeading3 = document.getElementById("selection3");
const selectionOptions3 = document.getElementById("selection-options3");
const genomeDiv = document.getElementById("genomeDiv");
const clusterDiv = document.getElementById("clusterDiv");
const nextButton = document.getElementById("next-btn"); 
const uuid = String(Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100))

var pipeline; 
var runOnce = true;
var genomeList = [];
var cluster; 
var fileContent;
var email;
var genomeSizeList;

const originalStyles = {
    backgroundColor: '#ffff',
    color: '#222',
    fontWeight: '500',
    width: '100%',
    border: '1px solid #222',
    padding: '10px',
    margin: '10px 0',
    //marginLeft: '200px',
    textAlign: 'center',
    borderRadius: '4px',
    cursor: 'pointer',
    transition: 'all 0.3s',
  };
  
  const hoverStyles = {
    backgroundColor: '#222',
    color: '#fff',
  };

function resetState(){
    //nextButton.style.display = "none";
    while(selectionOptions1.firstChild){
        selectionOptions1.removeChild(selectionOptions1.firstChild);

    }
    
}

function createGenomeButtons(genome){
    const radioButton = document.createElement("INPUT");
    radioButton.type = "checkbox";
    radioButton.value = genome;
    radioButton.name = "genome";
    radioButton.id = genome;
    radioButton.style.marginBottom = "8px";
    radioButton.style.marginRight = "5px";


    var label = document.createElement('label')
    label.htmlFor = genome;
    var description = document.createTextNode(genome);
    label.appendChild(description);
    label.style.marginBottom = "15px";
    

    var newLine = document.createElement('br');
    return [radioButton, label, newLine];
}

function createNextButton(){
    const button = document.createElement("button");
    button.innerHTML = "Next";
    button.value = "nextBtn";
    button.classList.add("btn");
    button.id = "nxtBtn";
    return button; 
}

function createVcfButtons(name,link){
    console.log("creating vcf button")
    const button = document.createElement("button");
    button.innerHTML = name;
    button.classList.add("btn");
    button.setAttribute('data-link',link);
    selectionOptions.appendChild(button);
    
}


function showClusters(){

    //resetState();
    clusterDiv.style.display = 'inline-block';
    selectionHeading3.innerHTML ="3. Choose Cluster";
    clusters.options.forEach(options => {

        const radioButton = document.createElement("INPUT");
        radioButton.type = "radio";
        radioButton.value = options.value;
        radioButton.name = 'clusterButton';
        radioButton.id = options.value;
        radioButton.style.marginBottom = "8px";
        radioButton.style.marginLeft = "36%";
    
    
        var label = document.createElement('label')
        label.htmlFor = options.text;
        var description = document.createTextNode(options.text);
        label.appendChild(description);
        label.style.marginBottom = "8px";
        label.style.marginLeft = "5px";
        //label.style.marginLeft = "36%";
        
    
        var newLine = document.createElement('br');
        
        selectionOptions3.appendChild(radioButton);
        selectionOptions3.appendChild(label);
        selectionOptions3.appendChild(newLine);

    });
    
    const button = document.createElement("button");
    button.innerHTML = 'View Resource Availability';
    button.classList.add("btn");
    button.id = "resourceAvlb";
    button.setAttribute('btn-data','resourceBtn');
    selectionOptions3.appendChild(button);
    
    const submitButton = document.createElement('input');
    submitButton.innerHTML = 'Submit Job';
    submitButton.classList.add("btn");
    submitButton.id = "submit";
    submitButton.type = 'submit';
    selectionOptions3.appendChild(submitButton);
    
    const resourceAvlb = document.getElementById('resourceAvlb');
    resourceAvlb.addEventListener('click', function(event) {
            //const externalURL = 'https://sites.google.com/view/raopraveen/sds-lab'; 
            const externalURL = 'http://clnode232.clemson.cloudlab.us:3000/d/fdafc397-6177-4291-86d0-73abf6715010/clustermetrics1?orgId=1&from=1697482314626&to=1697483214626'; 
            window.open(externalURL, '_blank');
    });

    const buttonWithId = document.querySelector('#nxtBtn');
    buttonWithId.addEventListener('click', function() {
        const radioButtons = document.querySelectorAll('input[type="radio"]:checked');
       
        radioButtons.forEach(function (radio) {
            if (radio.id) {
                cluster = radio.id;
            }
         });

        if(cluster){
            
            console.log("cluster is ",cluster);
            
            const subButton = document.getElementById('submit');
            subButton.addEventListener('click', function() {
                event.preventDefault();

                // const uuid = String(Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100))
                email = "khawar"
                if(email) {
                    //submit form
                    
                    event.preventDefault();
                    const csrftoken = document.querySelector('[name=csrfmiddlewaretoken]').value;
                        const customData = {
                            pipeline,
                            genomeList,
                            cluster,
                            fileContent,
                            email,
                            genomeSizeList,
                            uuid
                        };    
                    console.log("final data getting sent is ",customData);
                        fetch('', {
                            method: 'POST',
                            headers: {
                                'Content-Type': 'application/json', 
                                'X-CSRFToken': csrftoken,
                            },
                            body: JSON.stringify(customData),
                        })
                            .then(response => response.text())
                            .then(data => {            
                            })
                            .catch(error => {
                                console.error('Error:', error);
                            });
                       // processingGenome();
                       // checkFiles();
                }
                else{
                    event.preventDefault();
                   alert('input email address');
                }
            });
        }
        else{
            event.preventDefault();
            alert('Please select a cluster');
        }
    });
}

function submit(){
    //resetState();
    // selectionHeading3.innerHTML ="4. Enter Your Email Address";
    // const emailInput = document.createElement("INPUT");
    // emailInput.type = "email";
    // emailInput.id = "email";
    // emailInput.placeholder = "Enter your email";
    // emailInput.style.display = "block";
    // emailInput.style.marginLeft = "180px";
    // emailInput.style.marginTop = "25px";
    // emailInput.style.marginBottom = "25px";
    // emailInput.style.borderRadius = "5px";
    // emailInput.style.boxShadow = "2px 2px 4px rgba(0, 0, 0, 0.1)";
    // emailInput.style.border = "1px solid #ccc";
    // emailInput.style.textAlign = "center";
    // selectionOptions3.appendChild(emailInput);

    
    // const submitButton = document.createElement('input');
    // submitButton.innerHTML = 'Submit Job';
    // submitButton.classList.add("btn");
    // submitButton.id = "submit";
    // submitButton.type = 'submit';
    // selectionOptions3.appendChild(submitButton);
    
    // const subButton = document.getElementById('submit');
    // subButton.addEventListener('click', function() {
    //     event.preventDefault();
    //     email = document.getElementById("email").value;
    //     console.log('user email is ',email);
    //     // const uuid = String(Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100))
    //     if(email) {
    //         //submit form
    //         event.preventDefault();
    //         const csrftoken = document.querySelector('[name=csrfmiddlewaretoken]').value;
    //             const customData = {
    //                 pipeline,
    //                 genomeList,
    //                 cluster,
    //                 fileContent,
    //                 email,
    //                 genomeSizeList,
    //                 uuid
    //             };    
    //         console.log("final data getting sent is ",customData);
    //             fetch('', {
    //                 method: 'POST',
    //                 headers: {
    //                     'Content-Type': 'application/json', 
    //                     'X-CSRFToken': csrftoken,
    //                 },
    //                 body: JSON.stringify(customData),
    //             })
    //                 .then(response => response.text())
    //                 .then(data => {            
    //                 })
    //                 .catch(error => {
    //                     console.error('Error:', error);
    //                 });
    //             processingGenome();
    //             checkFiles();
    //     }
    //     else{
    //         event.preventDefault();
    //        alert('input email address');
    //     }
    // });
}

function checkFiles(){
    //call the end point to check for vcfs in hdfs
    //event.preventDefault();
    const csrftoken = document.querySelector('[name=csrfmiddlewaretoken]').value;
    console.log("uuid is ",uuid)    
    const customData = {
            uuid
        };    
    fetch('/checkFiles/',{
        method: 'POST',
        headers: {
            'Content-Type': 'application/json', 
            'X-CSRFToken': csrftoken,
        },
        body: JSON.stringify(customData),
    }) 
    //fetch('/checkFiles/') 
    .then(response => response.json())
    .then(data => {
        console.log('data stop is ',data.stop)
        console.log('shareable link is ',data.shareable_link)
        time = 300000; //5 minutes
        console.log('incoming uuid is ',data.uuid,' users uuid is ',uuid)
        if(data.uuid == uuid && data.shareable_link.length > 9){
        // if (data.shareable_link !== "False") {
            if(runOnce){
                resetState();  
            }
            runOnce = false;
            selectionHeading.innerHTML ="View VCFs generated";
            
            createVcfButtons(data.file_name,data.shareable_link);
            
            selectionOptions.addEventListener('click', function(event) {
                    event.preventDefault();
                    const targetButton = event.target;
                    const link = targetButton.dataset.link;
                    console.log("link is ",link)
                    const externalURL = link; 
                    window.open(externalURL, '_blank');
            });
            
            if(data.stop == false){
                console.log("i came here to stop condition block")
                return;
            } 
            setTimeout(arguments.callee,1000); // 1000 = 1 second

        }
        else if(data.stop == false){
            console.log("i came here to stop condition block")
            return;
        } 
        else {
            console.log("checking for file")
            setTimeout(arguments.callee,1000); // 1000 = 1 second
        }
 //   }

    })
}


function processingGenome(){
    resetState();
    selectionHeading.innerHTML ="Job Submitted. Processing Genome File";
    const getData = document.getElementById('img-srcs');
    const imgUrl = getData.getAttribute('data-genome-url')
    const image = document.createElement("img");
    image.src = imgUrl; 
    image.style.height= "200px";
    image.style.width= "auto";
    image.style.display="block";
    image.style.margin="auto";
    image.style.paddingLeft = "200px";
    image.alt = "genome loading"; 
    selectionOptions.appendChild(image);
}


///////////////////////
//////start here!!!////
///////////////////////

//resetState()
//create intials buttons for pipeline selection 
selectionHeading1.innerHTML ="1. "+selections.selection;
selections.options.forEach(options => {
    const button = document.createElement("button");
    button.innerHTML = options.text;
    button.name = "pipeline";
    button.value = options.value;
    button.setAttribute('btn-data','pipelineBtns');
    button.classList.add("btn");
    selectionOptions1.appendChild(button);
});

selectionOptions1.addEventListener('click', (e) => {
    const check = e.target.getAttribute('btn-data');
    var end = false; 
    //if(e.target.classList.contains('btn')) {
    if(check === 'pipelineBtns') {
        pipeline = e.target.value; 
        //check the value and present either radio buttons or file upload 
            //resetState();
            if(e.target.value === "svc"){
                
                //create upload file form
                //make second selection visible 
                
                genomeDiv.style.display = 'inline-block';
                selectionHeading2.innerHTML ="2. Upload File";
                
                const uploadFileBtn = document.createElement("INPUT");
                uploadFileBtn.type = "file";
                uploadFileBtn.id = "file";
                uploadFileBtn.style.display = "block";
                uploadFileBtn.style.marginLeft = "36%";
                uploadFileBtn.style.marginTop = "25px";
                uploadFileBtn.style.marginBottom = "25px";
                selectionOptions2.appendChild(uploadFileBtn);
                selectionOptions2.style.textAlign = "center";
                selectionOptions2.style.marginTop = "10px";

                //create next button 
                const button = document.createElement("button");
                button.innerHTML = "Next";
                button.value = "nextBtn";
                button.id = "nextBtn";
                Object.assign(button.style,originalStyles);

                selectionOptions2.appendChild(button)
                button.addEventListener('mouseover', () => {
                    Object.assign(button.style, hoverStyles);
                  });
                  
                  
                  // Add a mouseout event listener to restore original styles
                  button.addEventListener('mouseout', () => {
                    Object.assign(button.style, originalStyles);
                  });
                  event.preventDefault();
                const buttonWithId = document.querySelector('#nextBtn');

                buttonWithId.addEventListener('click', function() {
                    event.preventDefault();
                    const uploadFileBtn = document.getElementById("file"); // Replace with the correct ID
                    const fileInput = uploadFileBtn.files[0];

                    if (fileInput) {
                        // A file has been selected
                        console.log("File has been uploaded:", fileInput.name);
                        
                        const fileReader = new FileReader();
                        fileReader.onload = function (event) {
                            const fileContents = event.target.result; 
                            const lines = fileContents.split("\n");                    
                            fileContent = lines;
                        };
                    
                        // Read the file as text
                        fileReader.readAsText(fileInput);
                        
                        showClusters();
                     
                    } else {
                        event.preventDefault();
                        alert('Upload File');
                    }
                  });  
            }
            
            else{
                //create list of radio buttons 
                event.preventDefault();
                genomeDiv.style.display = 'inline-block';
                selectionHeading2.innerHTML ="2. Select Genome";
                const genomeNames = ["ERR016294","ERR016314","ERR016327","ERR016338"]; //"ERR016326",
                genomeSizeList = ["1924688643","1144451014","4091919508","2312019116"]; //"2962859843",
                
                for (let i =0; i<genomeNames.length; i++){
                    
                    ret = createGenomeButtons(genomeNames[i]);
                    radioButton = ret[0];
                    label = ret[1];
                    newLine = ret[2]; 
                    
                    selectionOptions2.style.textAlign = "center";
                    selectionOptions2.style.marginTop = "10px";
                    selectionOptions2.appendChild(radioButton);
                    selectionOptions2.appendChild(label);
                    selectionOptions2.appendChild(newLine);

                }
                

                //create next button
                button = createNextButton();
                selectionOptions2.appendChild(button)

                const buttonWithId = document.querySelector('#nxtBtn');

                buttonWithId.addEventListener('click', function() {
                    //alert('Button with class was clicked');
                    event.preventDefault();
                    const radioButtons = document.querySelectorAll('input[type="checkbox"]:checked');

                    radioButtons.forEach(function (radio) {
                        if (radio.id) {
                            genomeList.push(radio.id);
                        }
                    });

                    if(genomeList.length>0){

                    // while(selectionOptions2.firstChild){
                    //     selectionOptions2.removeChild(selectionOptions.firstChild);
                    // }

                    
                    showClusters();
                
                    }
                    else{
                        event.preventDefault();
                        alert('Please select atleast one genome to process');
                    }
                    
                  }); 

            }
        }
  });
