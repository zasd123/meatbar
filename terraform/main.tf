#Zachary Hansen 2022 
##############################
# PROVIDER SETUP 
##############################
terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.0"
    }
    docker={
        source= "kreuzwerker/docker"
        version="2.20.2"
    }
  }
  backend "s3" {
    bucket = "zhansen-terraform-states"
    region = "us-east-1" 
    key = "meatbar-qa"
    profile="qa"
  }
}
provider "aws" {
  region = var.region
  profile = var.env
}
variable "build_docker_image" {
    type = string
  
}

##############################
#DOCKER BUILD + PUSH TO ECR
##############################
resource "docker_image" "haskell"{
    depends_on = [
      aws_ecr_repository.service_repo
    ]
    #allows you to control wether a new build is required for deploying
    count = var.build_docker_image ? 1 : 0
    name="${var.aws_account_id}.dkr.ecr.${var.region}.amazonaws.com/${aws_ecr_repository.service_repo.name}"
    build {
        path = "../"
    }
    triggers = {
    dir_sha1 = sha1(join("", [for f in fileset(path.module, "./*") : filesha1(f)]))
  }
  provisioner "local-exec" {
    command = "ecr_password=`aws --profile ${var.env} ecr get-login-password --region ${var.region}` && docker login --username AWS --password $ecr_password ${var.aws_account_id}.dkr.ecr.${var.region}.amazonaws.com && docker push ${var.aws_account_id}.dkr.ecr.${var.region}.amazonaws.com/${aws_ecr_repository.service_repo.name}"
  }
}

##############################
# VPC + SG SETUP
##############################
resource "aws_vpc" "main" {
  cidr_block = "10.0.0.0/28"
  tags = {
    project = var.project,
    owner = var.owner
    environment = var.env
  }
}

resource "aws_subnet" "subnet_a" {
  vpc_id = aws_vpc.main.id
  availability_zone = "${var.region}a"
  cidr_block = "10.0.0.0/28"
  
}

resource "aws_vpc_endpoint" "ecr_api" {
  vpc_id       = aws_vpc.main.id
  service_name = "com.amazonaws.us-east-1.ecr.api"
  vpc_endpoint_type = "Interface"
}

resource "aws_vpc_endpoint" "ecr_dkr" {
  vpc_id       = aws_vpc.main.id
  service_name = "com.amazonaws.us-east-1.ecr.dkr"
  vpc_endpoint_type = "Interface"
}

resource "aws_vpc_endpoint" "s3" {
  vpc_id       = aws_vpc.main.id
  service_name = "com.amazonaws.us-east-1.s3"
}

resource "aws_vpc_endpoint" "logs" {
  vpc_id       = aws_vpc.main.id
  service_name = "com.amazonaws.us-east-1.logs"
  vpc_endpoint_type = "Interface"
}

resource "aws_internet_gateway" "gw" {
  vpc_id = aws_vpc.main.id

  tags = {
    project = var.project,
    owner = var.owner
    environment = var.env
  }
}

resource "aws_route_table" "example" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.gw.id
  }

  tags = {
    project = var.project,
    owner = var.owner
    environment = var.env
  }
}

resource "aws_security_group" "allow_tls" {
  name        = "allow_tls"
  description = "Allow TLS inbound traffic"
  vpc_id      = aws_vpc.main.id

  ingress {
    description      = "TLS from VPC"
    from_port        = 8080 
    to_port          = 8080 
    protocol         = "tcp"
    cidr_blocks      = ["0.0.0.0/0"]
  }

  egress {
    from_port        = 0
    to_port          = 0
    protocol         = "-1"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

  tags = {
    project = var.project,
    owner = var.owner
    environment = var.env
  }

}

resource "aws_cloudwatch_log_group" "ecs" {
  name = "${var.project}-${var.env}-ecs"
  tags = {
    project = var.project,
    owner = var.owner
    environment = var.env
  }

}

##############################
# IAM ROLE SETUP
##############################
resource "aws_iam_role" "test_role" {
  name = "${var.project}-${var.env}"
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Sid    = "1"
        Principal = {
          Service = "ecs-tasks.amazonaws.com"
        }
      },
      {
      Effect= "Allow"
      Principal= {
        AWS= "arn:aws:iam::${var.aws_account_id}:root"
      }
      Action= "sts:AssumeRole"
      Condition={}
     }
    ]
  })

  tags = {
    project = var.project,
    owner = var.owner
    environment = var.env
  }
}

resource "aws_iam_role_policy" "test_policy" {
  depends_on = [aws_iam_role.test_role, aws_ecs_cluster.cluster, aws_ecs_service.service_def, aws_ecs_task_definition.task_def, aws_ecr_repository.service_repo]
  name = "${var.project}-policy-${var.env}"
  role = aws_iam_role.test_role.id
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
            "Sid": "VisualEditor0",
            "Effect": "Allow",
            "Action": [
                "ecs:DiscoverPollEndpoint",
                "ecs:PutAccountSettingDefault",
                "ecs:CreateCluster",
                "ecs:DescribeTaskDefinition",
                "ecs:PutAccountSetting",
                "ecs:ListServices",
                "ecs:CreateCapacityProvider",
                "ecs:DeregisterTaskDefinition",
                "ecs:ListAccountSettings",
                "ec2:Describe*",
                "ecs:DeleteAccountSetting",
                "ecs:ListTaskDefinitionFamilies",
                "ecs:RegisterTaskDefinition",
                "ecs:ListTaskDefinitions",
                "ecs:CreateTaskSet",
                "ecs:ListClusters"
            ],
            "Resource":[
                "${aws_ecs_cluster.cluster.arn}", 
                "${aws_ecs_task_definition.task_def.arn}",
                "${aws_cloudwatch_log_group.ecs.arn}"
                ]
        },
        {
            "Sid": "VisualEditor1",
            "Effect": "Allow",
            "Action": "ecs:*",
            "Resource": [
                "arn:aws:ecs:*:${var.aws_account_id}:task-definition/*:*",
                "arn:aws:ecs:*:${var.aws_account_id}:task-set/*/*/*",
                "arn:aws:ecs:*:${var.aws_account_id}:container-instance/*/*",
                "arn:aws:ecs:*:${var.aws_account_id}:cluster/*",
                "arn:aws:ecs:*:${var.aws_account_id}:service/*/*",
                "arn:aws:ecs:*:${var.aws_account_id}:capacity-provider/*",
                "arn:aws:ecs:*:${var.aws_account_id}:task/*/*"
            ]
        },
       {
            "Sid": "VisualEditor2",
            "Effect": "Allow",
            "Action": [
                "logs:DescribeQueries",
                "logs:GetLogRecord",
                "logs:PutDestinationPolicy",
                "logs:StopQuery",
                "logs:TestMetricFilter",
                "logs:DeleteDestination",
                "logs:DeleteQueryDefinition",
                "logs:PutQueryDefinition",
                "logs:GetLogDelivery",
                "logs:ListLogDeliveries",
                "logs:CreateLogDelivery",
                "logs:DeleteResourcePolicy",
                "logs:PutResourcePolicy",
                "logs:DescribeExportTasks",
                "logs:GetQueryResults",
                "logs:UpdateLogDelivery",
                "logs:CancelExportTask",
                "logs:DeleteLogDelivery",
                "logs:DescribeQueryDefinitions",
                "logs:PutDestination",
                "logs:DescribeResourcePolicies",
                "logs:DescribeDestinations",
            ],
            "Resource": "${aws_cloudwatch_log_group.ecs.arn}"
        },
        {
        "Sid": "VisualEditor4",
        "Effect": "Allow",
        "Action": ["ecr:GetAuthorizationToken"],
        "Resource": "*"
       },
       {
        "Sid": "VisualEditor5",
        "Effect": "Allow",
        "Action": ["logs:CreateLogStream"],
        "Resource": "*"
       },
       {
        "Sid": "VisualEditor6",
        "Effect": "Allow",
        "Action": ["ecr:*"],
        "Resource": "${aws_ecr_repository.service_repo.arn}"
       },
       {
        "Sid": "VisualEditor7",
        "Effect": "Allow",
        "Action": ["codebuild:*","codepipeline:*"],
        "Resource": "*"
       }
    ]
  })
}
##############################
# ECR SETUP 
##############################

resource "aws_ecr_repository" "service_repo" {
  name                 = "${var.project}-${var.env}"
  image_tag_mutability = "MUTABLE"

  image_scanning_configuration {
    scan_on_push = true
  }
}

##############################
# ECS SETUP
##############################

resource "aws_ecs_cluster" "cluster" {
  name = "${var.project}-${var.env}-cluster"
}

resource "aws_ecs_task_definition" "task_def" {
  depends_on = [aws_iam_role.test_role]
  family = "${var.project}-${var.env}"
  container_definitions = jsonencode([
    {
      name      = "${var.project}-task" 
      image     = "${aws_ecr_repository.service_repo.repository_url}" 
      cpu       = 256 
      memory    = 1024
      essential = true
      portMappings = [
        {
          containerPort = 8080
          hostPort      = 8080
        }
      ],
      logConfiguration = {
        logDriver = "awslogs"
        options = {
            awslogs-group="${var.project}-${var.env}-ecs"
            awslogs-region= "${var.region}",
            awslogs-stream-prefix="ecs"
        }
       },
      
    }
  ])
  requires_compatibilities = ["FARGATE"] 
  network_mode             = "awsvpc"    
  memory                   = 1024         
  cpu                      = 256         
  execution_role_arn       = "${aws_iam_role.test_role.arn}"
}

resource "aws_ecs_service" "service_def" {
  depends_on = [aws_subnet.subnet_a]
  name            = "${var.project}-${var.env}-service"                             
  cluster         = "${aws_ecs_cluster.cluster.id}"             
  task_definition = "${aws_ecs_task_definition.task_def.arn}" 
  launch_type     = "FARGATE"
  desired_count   = 1

network_configuration {
    subnets          = [aws_subnet.subnet_a.id]
    assign_public_ip = true
    security_groups = [aws_security_group.allow_tls.id]
  }
}