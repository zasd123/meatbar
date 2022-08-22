#*****Zach Hansen's Documentation*****#

BACKGROUND
I received the original request above linking me to this repository: https://github.com/freckle/meatbar which either does not exist or is private. 

I didn't have a lot of free time to work on this so instead of contacting the recruiter over the weekend, I did some searching around on Github and found this repo: https://github.com/stackptr/meatbar which appears to match up to the project requirements.  If you want this work in a pull request on the freckle repo, just grant my access and let me know.
My public Github username is: zasd123


GENERAL ARCHITECTURE OVERVIEW

VPC/Subnets
- Allocated a /28 ip address VPC with a single subnet to host my ECS cluster

Networking/Security: 
- VPC endpoints for ECS dependent services along 
- Internet Gateway along with an associated route-table entry
- A non-default security group following least privilege for the web-app

Iam:
- Application specific IAM role for use by the ECS service 
- Policy document that allows all the necessary permissions for the ECR push/pull, logging and deployment + execution of the application.

Image Repository:
- ECR was an obvious choice here as it integrates well with all the AWS conainter hosting services. 

Application Hosting:
- ECS/Fargate was selected to host the single container application due to ease of integration with AWS IAM for permissions 
- If this was just an API, I would have chose docker on Lambda triggered by an API-gateway.
- Docker on EC2 wouldn’t be a bad choice here either but I decided that an ECS setup would have less setup time due to the "serverless" design
- EKS was also considered, but I didn’t see a need all that granularity considering its just a single container application without load balancing, etc.

Logging:
- Cloudwatch agent is downloaded and configured during the docker build process,  /cloudwatch/amazon-cloudwatch-agent.json just needs to be pointed to the application log file in the guest OS

ENDPOINT/DNS
The application is current hosted at 18.232.145.101:8080. I don’t have any spare Domain names at the moment so I am skipping DNS and SSL setup for now, however I could easily set up an A/PTR record with AWS Route53 and a TLS cert in ACM via Terraform. This site will stay up until friday 08/26/22 as I am already mostly maxed out of free-teir resources in AWS.

terraform/main.tf handles the following:
-Docker build 
-Upload to ECR, 
-Provisioning of infrastructure
-Hot-swap deployment of new images to the ECS task 

DEPLOYMENT
Pre-requisites:
- A terraform client and Docker daemon/desktop installed locally
- An existing s3 bucket to use as the terraform backend. Local state storage is also an option. 
- A valid local AWS profile that matches the env var in qa.tfvars
    - $aws --profile qa configure 
    - <enter access key and secret>

terraform/main.tf handles the following:
-Docker build 
-Upload to ECR, 
-Provisioning of infrastructure
-Hot-swap deployment of new images to the ECS task 

Instructions (from the terraform directory):
-$terraform init
-$terraform apply -var-file qa.tfvars
- When run with the qa var file, there is an empty varible "build_docker_image" that controls wether or not a new image built and uploaded to ECR
   - command below can me used to force the build without command line access:
   - terraform apply -var-file qa.tfvars -var="build_docker_image=1"

Changes I would make given more time/resources:
- Automated unit testing on deployment 
- AWS WAF to better monitor and control the traffic that reaches the endpoint
- Load balancing and another subnet in a different region to allow high availability and high scalability for the ECS cluster
- Container scanning, beyond what ECR does automatically
- An AWS codepipeline triggered by a webook from Git to build and deploy automatically on commit/release.
- Rework the Docker build step which compiles the Haskell stack, it’s taking roughly 8 minutes to do so on my local machine and couldn’t figure out exactly why.  This is the reason for the build_docker_image variable
