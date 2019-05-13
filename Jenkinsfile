def CONTAINER_NAME=""
def CONTAINER_TAG="latest"
def HTTP_PORT="8081"


node {

  def DRC_PATH="/root/jenkins/jenkins_home/workspace/backend"
  env.NODEJS_HOME = "${tool 'node'}"
  env.PATH="${env.NODEJS_HOME}/bin:${env.PATH}"
  currentBuild.result = 'SUCCESS'
  boolean skipBuild = false

  stage('Initialize'){
    def dockerHome = tool 'myDocker'
  }


  def branch = 'master'

  stage('Checkout') {
    def scmVars = checkout scm
    def git_branch = scmVars.GIT_BRANCH

    if (git_branch == 'origin/development'){
      branch = 'development'
    }
  }

  try {

    stage("Image Prune"){
      imagePrune(DRC_PATH, branch)
    }

    stage('Run App'){
      runApp(CONTAINER_NAME, CONTAINER_TAG, HTTP_PORT, DRC_PATH, branch)
    }
  } catch (err) {
    currentBuild.result = 'FAILED'
    throw err
  }

}

def imagePrune(DRC_PATH, branch){
    try {
        sh "docker-compose -f docker-compose.${branch}.yml --project-directory=${DRC_PATH}_${branch} down -v"
        sh "docker-compose -f docker-compose.${branch}.yml --project-directory=${DRC_PATH}_${branch} rm -f "
    } catch(error){}
}

def runApp(containerName, tag, httpPort, DRC_PATH, branch){

  withCredentials([string(credentialsId:  'MAILCHIMP_API', variable: 'MAILCHIMP_API')]) {
    sh "MAILCHIMP_API=$MAILCHIMP_API docker-compose -f docker-compose.${branch}.yml --project-directory=${DRC_PATH}_${branch}  up -d "
  }

  echo "Application started on port: ${httpPort} (http)"
}

