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
const popupLink = document.getElementById("infoButton");
const popupWindow = document.getElementById("popup-window");
const closeButton = document.getElementById("close-button");
const pipelineDiv = document.getElementById("pipelineDiv");
const genomeDiv = document.getElementById("genomeDiv");
const clusterDiv = document.getElementById("clusterDiv");
const nextButton = document.getElementById("next-btn"); 
var uuid = uuidv4().replace(/-/g, '');
console.log("generated v4 uuid is ",uuid," of length ",uuid.length);

var flag = true;
var pipeline; 
var runOnce = true;
var genomeList = [];
var cluster; 
var email;
var allGenomeSizes;
var genomeSizeList = [];
var vcfTotal = 0;
var result; 
var clusterUsages = [];
var accessionIDs; 

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
    button.classList.add("hover-effect");
    button.id = "nxtBtn";
    return button; 
}

function createVcfButtons(name,link){
    console.log("creating vcf button")
    
    var label = document.createElement('label')
    var description = document.createTextNode(name);
    label.appendChild(description);
    label.style.marginBottom = "8px";
    label.style.marginRight = "5px";
    label.style.marginLeft = "-44px";


    const button = document.createElement("button");
    button.innerHTML = 'Download';
    button.classList.add("btn"); 
    button.classList.add("hover-effect");
    button.setAttribute('data-link',link);
    button.style.display = "inline-block";
    button.style.width = "22%";
    button.style.borderRadius = "12px";
    button.style.padding = "5px";

    const id1 = 'igv1';
    const id2 = 'igv2';

    const button2 = document.createElement("button");
    button2.innerHTML = "View by IGV";
    button2.style.borderRadius = "9px";
    button2.classList.add("hover-effect");
    button2.classList.add("igvBtn");
    if(name == 'ERR016294.vcf'){
        button2.id = id1;
    }
    else{
        button2.id=id2;
    }
    var newLine = document.createElement('br');

    selectionOptions2.appendChild(label);
    selectionOptions2.appendChild(button);
    selectionOptions2.appendChild(button2);
    selectionOptions2.appendChild(newLine);
}


function submitWithEmailUsername() {
    console.log("CLUSTER IS ",cluster);
   // buttonWithId.style.backgroundColor = "lightgray";
    const buttons = document.querySelectorAll("#selection-options3 .btn2")
    buttons.forEach(function(button) {
        button.classList.remove("hover-effect");
        button.disabled = true;
      });
    
    var radioButtons = document.querySelectorAll('input[type="radio"]');
      radioButtons.forEach(function (radio) {
          radio.disabled = true;
          console.log("disabled radio buttons");
      });  

    const autoBtn = document.getElementById('autoBtnID');

    //hide other divs
      window.requestAnimationFrame(function() {
        autoBtn.style.display = 'none';
        pipelineDiv.style.visibility = 'hidden';
        clusterDiv.style.visibility = 'hidden';
      });
    

    // email/username input 
    selectionHeading2.textContent = "4. Enter your name"
    //reset previous content 
    while(selectionOptions2.firstChild){
        selectionOptions2.removeChild(selectionOptions2.firstChild);
    }
    //create email text field and next button 
    
    const emailInput = document.createElement("INPUT");
    emailInput.type = "email";
    emailInput.id = "email";
    emailInput.placeholder = "Enter your name";
    emailInput.style.display = "block";
    emailInput.style.marginLeft = "190px";
    emailInput.style.marginTop = "25px";
    emailInput.style.marginBottom = "25px";
    emailInput.style.borderRadius = "5px";
    emailInput.style.boxShadow = "2px 2px 4px rgba(0, 0, 0, 0.1)";
    emailInput.style.border = "1px solid #ccc";
    emailInput.style.textAlign = "center";
    selectionOptions2.appendChild(emailInput);

    
    const submitButton = document.createElement('input');
    submitButton.innerHTML = 'Submit Job';
    submitButton.classList.add("btn");
    submitButton.classList.add("hover-effect");
    submitButton.id = "submit";
    submitButton.type = 'submit';
    selectionOptions2.appendChild(submitButton);

    submitButton.addEventListener('click',function(){
      
        email = emailInput.value;
        if(email){
            
            console.log("EMAIL IS ",email);
            emailInput.disabled = true;
            
            //change heading nad empty the content of div 
            selectionHeading2.textContent = "5. Processing Genome. Cluster ("+cluster.charAt(2)+")";
            selectionHeading2.textContent = "5. Processing Genome. Cluster (1)";
            while(selectionOptions2.firstChild){
                selectionOptions2.removeChild(selectionOptions2.firstChild);
            }

            const getData = document.getElementById('img-srcs');
            const imgUrl = getData.getAttribute('data-genome-url')
            const image = document.createElement("img");
            image.src = imgUrl; 
            image.id = "genomeLoading";
            image.style.height= "200px";
            image.style.width="auto";
           image.style.display="block";
            image.style.margin="auto";
           
            image.alt = "genome loading"; 
            selectionOptions2.appendChild(image);

            //need to send post request here. 
            //const uuid = String(Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100))
            const csrftoken = document.querySelector('[name=csrfmiddlewaretoken]').value;
            console.log(genomeList)
            console.log(genomeSizeList)
            // uuid = '1923'
            const customData = {
                    pipeline,
                    genomeList,
                    cluster,
                    email,
                    genomeSizeList,
                    uuid,
                    accessionIDs
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
                    .then(data => { })
                    .catch(error => {
                    console.error('Error:', error);
                    });
                    // processingGenome();
                     checkFiles();
        
            event.preventDefault();
        }
        else{
            alert("Enter Email Address")
            event.preventDefault();
        }
    });

    event.preventDefault();
}


function submit() {
    console.log("CLUSTER IS ",cluster);
   // buttonWithId.style.backgroundColor = "lightgray";
    const buttons = document.querySelectorAll("#selection-options3 .btn2");
    buttons.forEach(function(button) {
        button.classList.remove("hover-effect");
        button.disabled = true;
      });
    
    var radioButtons = document.querySelectorAll('input[type="radio"]');
      radioButtons.forEach(function (radio) {
          radio.disabled = true;
          console.log("disabled radio buttons");
      });  

    const autoBtn = document.getElementById('autoBtnID');

    //hide other divs
      window.requestAnimationFrame(function() {
        autoBtn.style.display = 'none';
        pipelineDiv.style.visibility = 'hidden';
        clusterDiv.style.visibility = 'hidden';
      });

        selectionHeading2.textContent = "4. Processing Genome. Cluster ("+cluster.charAt(2)+")";
        selectionHeading2.textContent = "4. Processing Genome. Cluster (1)";
        while(selectionOptions2.firstChild){
            selectionOptions2.removeChild(selectionOptions2.firstChild);
        }

        const getData = document.getElementById('img-srcs');
        const imgUrl = getData.getAttribute('data-genome-url');
        const image = document.createElement("img");
        image.src = imgUrl; 
        image.id = "genomeLoading";
        image.style.height= "200px";
        image.style.width="auto";
        image.style.display="block";
        image.style.margin="auto";
        
        image.alt = "genome loading"; 
        selectionOptions2.appendChild(image);

        //need to send post request here. 
        //const uuid = String(Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100)+Math.floor(Math.random() * 100))
        const csrftoken = document.querySelector('[name=csrfmiddlewaretoken]').value;
        console.log(genomeList);
        console.log(genomeSizeList);
        // uuid = '1923'
        const customData = {
                pipeline,
                genomeList,
                cluster,
                email,
                genomeSizeList,
                uuid,
                accessionIDs
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
                .then(data => { })
                .catch(error => {
                console.error('Error:', error);
                });
                // processingGenome();
                    checkFiles();
    
        event.preventDefault();

};


setInterval(checkFiles, 20000); // time in miliseconds, 10 seconds = 10000

async function showClusters(){

    //resetState();

    clusterDiv.style.display = 'inline-block';
    selectionHeading3.innerHTML ="3. Choose Cluster";
    var i = 1 ;
    var i = 0 ;
    clusterUsages = [10,88,76]
    
    const csrftoken = document.querySelector('[name=csrfmiddlewaretoken]').value;
    // var clusterUsages = []; 


    //cluterUtilisation progress bars
    // try {
    //     pass 
    //     const response = await fetch('/getClusterUtilisations/',{
    //         method: 'GET',
    //         headers: {
    //             'Content-Type': 'application/json', 
    //             'X-CSRFToken': csrftoken,
    //         },
    //     }) 
        
    //     const data = await response.json();
    //     const result = JSON.parse(data.result); 
    //     console.log("resut is ",result);
    //     clusterUsages = clusterUsages.concat(result); 
    //     console.log("clus usages inside = ",clusterUsages)
 
    // }
    // catch (error) {
    //     console.error('Error fetching data:', error);
    // }
    
    

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

        

        var progressContainerDiv = document.createElement("div");
        progressContainerDiv.classList.add("progress-container");

        var progressBar = document.createElement("div");
        progressBar.classList.add("progress-bar"); 
        progressBar.id = "myProgressBar";
        progressBar.style.width = Math.ceil(clusterUsages[i]) + "%";
        progressBar.innerHTML = Math.ceil(clusterUsages[i]) + "%";
        i+=1; 
        //progressBar.insertAdjacentText('beforeend', '50%');

        progressContainerDiv.appendChild(progressBar);
        
        
        
        selectionOptions3.appendChild(radioButton);
        selectionOptions3.appendChild(label);
        //selectionOptions3.appendChild(newDiv);
        selectionOptions3.appendChild(progressContainerDiv);
        //selectionOptions3.insertAdjacentText('beforeend',"50%");
        selectionOptions3.appendChild(newLine);

    });
    
    const button = document.createElement("button");
    button.innerHTML = 'View Resource Usage';
    button.classList.add("btn");
    button.classList.add("hover-effect");
    button.id = "resourceAvlb";
    button.setAttribute('btn-data','resourceBtn');
    selectionOptions3.appendChild(button);
    
    const button2 = document.createElement("button");
    button2.innerHTML = 'Auto Select';
    button2.id = "autoBtnID";
    button2.classList.add("btn");
    button2.classList.add("hover-effect");
    selectionOptions3.appendChild(button2);

    const resourceAvlb = document.getElementById('resourceAvlb');
    resourceAvlb.addEventListener('click', function(event) { 
            const externalURL = 'http://c220g5-111307.wisc.cloudlab.us:3000/d/fdafc397-6177-4291-86d0-73abf671501000/clustermetricsdemo?orgId=1&refresh=5s'; 
            window.open(externalURL, '_blank');
            event.preventDefault();
    });
    

    const nxtButton = document.createElement("button");
    nxtButton.innerHTML = "Next";
    nxtButton.value = "nextBtn";
    nxtButton.id = "nextBtn2";
    nxtButton.classList.add("btn2");
    nxtButton.classList.add("hover-effect");
    selectionOptions3.appendChild(nxtButton)
    //event.preventDefault();

    //next button pressed
    const buttonWithId = document.querySelector('#nextBtn2');
    const autoSelectionBtn = document.querySelector('#autoBtnID');

    selectionOptions3.addEventListener('change',function(event){
        if (event.target.type==='radio'){
            autoSelectionBtn.style.backgroundColor = 'lightgray';
            autoSelectionBtn.disabled = true;
            autoSelectionBtn.classList.remove("hover-effect");
            autoSelectionBtn.style.borderColor = "lightgray";
            autoSelectionBtn.style.opacity = "60%";
            autoSelectionBtn.style.cursor ="default";
        }
    })

    buttonWithId.addEventListener('click', function() {
        //event.preventDefault();
        var radioButtons = document.querySelectorAll('input[type="radio"]:checked');
        radioButtons.forEach(function (radio) {
            if (radio.id) {
                cluster = radio.id;
            }
         });
        if(cluster){
            event.preventDefault();
            buttonWithId.style.backgroundColor = "lightgray";
            submit();
        }
        else{
            event.preventDefault();
            alert('Please select a cluster');
        }
    });

    autoSelectionBtn.addEventListener('click', function() {
        cluster = "auto";
        submit(); 
    });
}

function displayIGV(){
    var igvDiv = document.getElementById("selection-options3");
    while(selectionOptions3.firstChild){
        selectionOptions3.removeChild(selectionOptions3.firstChild);

    }
    selectionHeading3.style.display = 'none';
    clusterDiv.style.visibility = 'visible';
    
    var options =
      {
          genome: "hg38",
          locus: "chr8:127,736,588-127,739,371",
          tracks: [
            
                {
                    type: "variant",
                    format: "vcf",
                    url: "https://s3.amazonaws.com/1000genomes/release/20130502/ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz",
                    indexURL: "https://s3.amazonaws.com/1000genomes/release/20130502/ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz.tbi",
                    name: "1KG variants (chr22)",
                    squishedCallHeight: 1,
                    expandedCallHeight: 4,
                    displayMode: "squished",
                    visibilityWindow: 1000
                  }
            
          ]
      };
      console.log("came here flag is ",flag)
      if(flag){
        flag = false;
      igv.createBrowser(igvDiv, options)
              .then(function (browser) {
                  console.log("Created IGV browser");
              })
            }
}


function displayPhyloTree(){
    while(selectionOptions3.firstChild){
        selectionOptions3.removeChild(selectionOptions3.firstChild);
    }
    selectionHeading3.style.display = 'none';
    clusterDiv.style.visibility = 'visible';

    var newWindow = window.open("","phylogeny image", "_blank");
    var imgDiv = document.createElement('div');
    imgDiv.style.position = 'absolute';
    imgDiv.style.left = '40%';
    imgDiv.style.top = '40%';

    animationPath = "/static/images/animate.gif";
    var animationImg = document.createElement('img');
    animationImg.style.width = "400px";
    animationImg.src = animationPath;


    //newWindow.document.body.appendChild(animationImg);
    newWindow.document.body.appendChild(imgDiv);
    imgDiv.appendChild(animationImg);

    setTimeout(function() {
        console.log("im inside the timeout");
        var treeImg = document.createElement('img');
        var treeImgPath = "/static/images/phylogeny.jpeg"; 
        treeImg.src = treeImgPath;

        imgDiv.style.position = '';
        imgDiv.style.left = '';
        imgDiv.style.top = '';
 
        imgDiv.removeChild(animationImg);
        imgDiv.appendChild(treeImg);

    }, 25000); // 5000 milliseconds = 5 seconds

    console.log("skipped timeout block");

    //newWindow.document.body.appendChild(treeImg);
    //clusterDiv.appendChild(treeImg);
}

function showImage(){

}


function checkFiles(){
    //call the end point to check for vcfs in hdfs
    //event.preventDefault();

    const csrftoken = document.querySelector('[name=csrfmiddlewaretoken]').value;
    console.log("uuid is ",uuid)   ;
    const customData = {
            uuid,
            genomeList,
            email, 
            vcfTotal
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
        result = JSON.parse(data.result);
        console.log("result from server is")
        console.log(result);
        //loop over the results array 
        //div is selection Options2 
        //if vcf == 0 then create progress bars 
        //else, create buttons with links
        if(result.length > 0){
            ///stop the genome loading animation 
            //const imgGenomeLoading = document.getElementById('genomeLoading');
            //imgGenomeLoading.style.display = "none"; 
            
            selectionOptions2.innerHTML = ''
            
            var allVCFs;
            result.forEach(function(element){
                var genomeName = element['genome']; 
                var genome_vcf = element['vcf']; 

                if(genome_vcf==1){
                //read the link 
                //generate button 
                    
                    var link = element['link'];
                    createVcfButtons(genomeName,link);
                    selectionOptions2.addEventListener('click', function(event) {
                    const targetButton = event.target;
                    if(targetButton.dataset.link){
                                event.preventDefault();
                                const targetButton = event.target;
                                const link = targetButton.dataset.link;
                                console.log("link is ",link)
                                const externalURL = link; 
                                window.open(externalURL, '_blank');
                                console.log("vcf button pressed");
                            }
                        });
                        var igvBtn1 = document.getElementById("igv2");
                        igvBtn1.addEventListener('click', function(event) {
                            event.preventDefault();
                            console.log("igv button pressed");
                            displayIGV();
                        });


                        //if all vcfs generated, create phylogeny tree button
                        allVCFs = result.every(function(obj){
                        return obj['vcf'] === 1;
                    });

                    console.log("vcfs generated count : ",allVCFs)

                } //end if genome_vcf==1 block 

                else{
                //sum stages to check progress 
                //draw progress bars 
                    var stageSum = element['BAM'] + element['BWAMarkDuplicates'] + element['SortSam'] + element['GATK_BQSR'] + element['GATK_HaplotypeCaller'];
                    


                    var label = document.createElement('label')
                    label.htmlFor = genomeName;
                    var description = document.createTextNode(genomeName);
                    label.appendChild(description);
                    label.style.marginBottom = "8px";
                    label.style.marginLeft = "5px";
                    
                    var newLine = document.createElement('br');
                    
                    var progressContainerDiv = document.createElement("div");
                    progressContainerDiv.classList.add("progress-container");
            
                    var progressBar = document.createElement("div");
                    progressBar.classList.add("progress-bar"); 
                    progressBar.id = "myProgressBar";

                    progressBar.style.width = stageSum*20+"%";
                    
                    progressBar.innerHTML = stageSum*20+"%";
                        
                        
                    

                    switch(stageSum){
                        case 1:
                            progressBar.style.backgroundColor = "#cd624a"; 
                            progressBar.style.backgroundColor = "rgb(232,34,34)";
                            break;
                        case 2:
                            progressBar.style.backgroundColor = "#eda85a";
                            progressBar.style.backgroundColor = "rgb(237,75,135)";
                            break;
                        case 3:
                            progressBar.style.backgroundColor = "#eda85a";
                            break;
                        case 4:
                            progressBar.style.backgroundColor = "#47d096";
                            break;    
                        case 5:
                            progressBar.style.backgroundColor = "#cd624a";
                            progressBar.style.backgroundColor = "rgb(76,175,80)";
                            break;    
                    }
            
                    progressContainerDiv.appendChild(progressBar);


                    var label2 = document.createElement('label')
                    var description = document.createTextNode('Stage '+stageSum+'/5 completed');
                    label2.appendChild(description);
                    label2.style.marginBottom = "8px";
                    label2.style.marginLeft = "5px";
                    
                    var newLine = document.createElement('br');
                    
                    //selectionOptions2.style.display="grid"; 
                    selectionOptions2.appendChild(label);
                    selectionOptions2.appendChild(progressContainerDiv);
                    selectionOptions2.appendChild(label2);
                    selectionOptions2.appendChild(newLine);
                }
            })

            //all the vcfs have been generated 
            allVCFs = true;
            if (allVCFs == true) {
                //create phylogeny tree button
                button = createNextButton(); 
                button.text = "see phylogeny tree";
                button.style.width = "50%";
                button.id = "phyloButton";
                button.style.borderRadius = "14px";
                button.innerHTML = "Generate Phylogeny Tree"; 
                button.style.marginTop = "150px";
                selectionOptions2.append(button);

                var phyloBtn = document.getElementById("phyloButton");
                phyloBtn.addEventListener('click', function(event) {
                    event.preventDefault();
                    console.log("phylo generate btn pressed");
                    displayPhyloTree();
                });

            }

        }

    })
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
    button.classList.add("hover-effect");
    selectionOptions1.appendChild(button);
});


popupLink.addEventListener("click", function(event) {
    event.preventDefault();
    popupWindow.style.display = "block";
  });
  // Hide the pop-up window when the close button is clicked
  closeButton.addEventListener("click", function() {
    event.preventDefault();
    popupWindow.style.display = "none";
  });

selectionOptions1.addEventListener('click', (e) => {
    const check = e.target.getAttribute('btn-data');
    var end = false; 
    //if(e.target.classList.contains('btn')) {
    if(check === 'pipelineBtns') {
        pipeline = e.target.value; 
        e.target.style.backgroundColor = "lightgray";
        e.target.style.borderColor = "lightgray";
        e.target.style.opacity = "60%";
        

        const buttons = document.querySelectorAll("#selection-options1 .btn")
        buttons.forEach(function(button) {
            button.classList.remove("hover-effect");
            button.disabled = true;
            button.style.cursor ="default";
          });

        // e.target.classList.remove("hover-effect");
            if(e.target.value === "svc"){
                pipeline = e.target.value;
                console.log("PIPELINE IS ",pipeline); 

                genomeDiv.style.display = 'inline-block';
                selectionHeading2.innerHTML ="2. Select Genomes";
                
                const genomeNames = ["TCRBOA1","TCRBOA2","TCRBOA3","TCRBOA4","TCRBOA5"];
                allGenomeSizes = ["3500000000","4500000000","4000000000","3900000000","3800000000"];
            

                for (let i =0; i<genomeNames.length; i+=1){
                    
                    ret = createGenomeButtons(genomeNames[i]);
                    radioButton = ret[0];
                    label = ret[1];
                    label.style.marginRight = "15px";
                    newLine = ret[2]; 
                    
                    selectionOptions2.style.textAlign = "center";
                    selectionOptions2.style.marginTop = "10px";
                    selectionOptions2.appendChild(radioButton);
                    selectionOptions2.appendChild(label);
                    selectionOptions2.appendChild(newLine);
                    console.log("an edit");

                }

                //create next button 
                const button = document.createElement("button");
                button.innerHTML = "Next";
                button.value = "nextBtn";
                button.id = "nextBtn";
                button.classList.add("btn2");
                button.classList.add("hover-effect");
                selectionOptions2.appendChild(button)
                event.preventDefault();
                const buttonWithId = document.querySelector('#nextBtn');

                buttonWithId.addEventListener('click', function() {
                
                    var radioButtons = document.querySelectorAll('input[type="checkbox"]:checked');

                    radioButtons.forEach(function (radio) {
                        if (radio.id) {
                            genomeList.push(radio.id);
                        }
                    });
                

                    if(genomeList.length>0){
                    
                    buttonWithId.style.backgroundColor = "lightgray";
                    buttonWithId.disabled = true;
                    buttonWithId.classList.remove("hover-effect");

                    const buttons = document.querySelectorAll("#selection-options1 .btn")
                    buttons.forEach(function(button) {
                            button.classList.remove("hover-effect");
                            button.disabled = true;
                          });
                    
                    var radioButtons = document.querySelectorAll('input[type="checkbox"]');
                    radioButtons.forEach(function (radio) {
                        radio.disabled = true;
                    });
                    

                    genomeList.forEach(function(element){
                        index = genomeList.indexOf(element);
                        genomeSizeList.push(allGenomeSizes[index]);
                    })
                    console.log("genomeList is ",genomeList);
                    console.log("size list is ", genomeSizeList);


                    buttonWithId.style.backgroundColor = "lightgray";
                    buttonWithId.style.borderColor = "lightgray";
                    buttonWithId.style.opacity = "60%";
                    buttonWithId.style.cursor = "default";

                    showClusters();

                }
                else{
                    event.preventDefault();
                    alert('Please select atleast one genome to process'); 
                }
                    
                }); //end event listner 
            

                } //if svc 

            else{
                //create list of radio buttons 
                pipeline = e.target.value;
                console.log("PIPELINE IS ",pipeline);
                e.target.style.backgroundColor = "lightgray";
                genomeDiv.style.display = 'inline-block';
                selectionHeading2.innerHTML ="2. Select Genome(s)";
                //const genomeNames = ["ERR016294","ERR016314","ERR016327","ERR016338"]; //"ERR016326",
                const genomeNames = ["ERR016314","ERR016316","ERR016317","ERR016320","ERR016326","ERR016327","ERR016338","ERR016344","ERR016350","ERR018197","ERR018198","ERR018204","ERR018395","ERR018416","ERR018423","ERR018429","ERR018435","ERR018436","ERR018442","ERR018448","ERR018454","ERR018460","ERR018463","ERR018469","ERR018475","ERR018476","ERR018482"];
                allGenomeSizes = ["1144451014","1316511682","1253702634","1712582059","2962859843","4091919508","2312019116","2441875387","1405628913","2769601391","2940489805","2834987902","1155966013","4642960047","2074564581","1802900012","2074804536","1810796785","4238943221","1889860913","4780263286","2262114215","2164110037","2239111491","2211401602","7356216287","2170573337"];
            

                for (let i =0; i<genomeNames.length; i+=3){
                    
                    ret = createGenomeButtons(genomeNames[i]);
                    radioButton = ret[0];
                    label = ret[1];
                    label.style.marginRight = "15px";
                    newLine = ret[2]; 
                    
                    selectionOptions2.style.textAlign = "center";
                    selectionOptions2.style.marginTop = "10px";
                    selectionOptions2.appendChild(radioButton);
                    selectionOptions2.appendChild(label);

                    ret = createGenomeButtons(genomeNames[i+1]);
                    radioButton = ret[0];
                    label = ret[1]; 
                    label.style.marginRight = "15px";
                    selectionOptions2.appendChild(radioButton);
                    selectionOptions2.appendChild(label);

                    ret = createGenomeButtons(genomeNames[i+2]);
                    radioButton = ret[0];
                    label = ret[1];
                    label.style.marginRight = "15px";
                    selectionOptions2.style.textAlign = "center";
                    selectionOptions2.style.marginTop = "10px";
                    selectionOptions2.appendChild(radioButton);
                    selectionOptions2.appendChild(label);

                    selectionOptions2.appendChild(newLine);

                }
                
                //create next button
                var button = createNextButton();
                selectionOptions2.appendChild(button);
                const buttonWithId = document.querySelector('#nxtBtn');
                
                var customLabel1 = document.createElement('label');
                customLabel1.innerHTML = "or";
                selectionOptions2.appendChild(customLabel1);

                // Create container div
                var containerDiv = document.createElement('div');          
                
                var customLabel2 = document.createElement('label');
                customLabel2.innerHTML = 'Input Accession IDs'; // Custom text
                customLabel2.style.fontWeight = 'bold'; // Custom text
                customLabel2.style.display = "inline-block";

                
                var inputField = document.createElement('input');
                inputField.type = 'text'; 
                inputField.placeholder = 'ERR123..,ERR...,SRR...'; 
                inputField.style.display = "inline-block";
                inputField.style.border = "1px";
                inputField.style.borderStyle = "double";
                inputField.style.marginLeft = "5px"; 
                
             
                containerDiv.appendChild(customLabel2);
                containerDiv.appendChild(inputField);
                containerDiv.style.marginTop = "10px";
                containerDiv.style.paddingLeft = "50px";

                selectionOptions2.appendChild(containerDiv);
                selectionOptions2.style.textAlign = "center";
                selectionOptions2.style.marginTop = "10px";


                buttonWithId.addEventListener('click', function() {
                    //alert('Button with class was clicked');
                    //event.preventDefault();
                    var radioButtons = document.querySelectorAll('input[type="checkbox"]:checked');

                    radioButtons.forEach(function (radio) {
                        if (radio.id) {
                            genomeList.push(radio.id);
                        }
                    });
                    
                    accessionIDs = inputField.value; 
    
                    console.log(" inputField val is ",accessionIDs);

                    if(genomeList.length>0 || accessionIDs.length > 1){
                    
                    buttonWithId.style.backgroundColor = "lightgray";
                    buttonWithId.disabled = true;
                    buttonWithId.classList.remove("hover-effect");

                    inputField.disabled = true; 
                    inputField.style.backgroundColor = "lightgray";
                    inputField.style.borderColor = "lightgray";
                    inputField.style.opacity = "60%";

                    const buttons = document.querySelectorAll("#selection-options1 .btn")
                    buttons.forEach(function(button) {
                            button.classList.remove("hover-effect");
                            button.disabled = true;
                            button.style.cursor = "default"; 
                          });
                    
                    
                    var radioButtons = document.querySelectorAll('input[type="checkbox"]');
                    radioButtons.forEach(function (radio) {
                        radio.disabled = true;
                    });
                    
                
                    genomeList.forEach(function(element){
                        index = genomeList.indexOf(element);
                        genomeSizeList.push(allGenomeSizes[index]);
                    })

                    console.log("GENOME LIST IS ", genomeList);
                    console.log("SIZE LIST IS ", genomeSizeList);


                    buttonWithId.style.backgroundColor = "lightgray";
                    buttonWithId.style.borderColor = "lightgray";
                    buttonWithId.style.opacity = "60%";
                    buttonWithId.style.cursor = "default";


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
