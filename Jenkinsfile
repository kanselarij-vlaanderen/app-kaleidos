def CONTAINER_NAME=""
def CONTAINER_TAG="latest"
def HTTP_PORT="8081"

properties([
    parameters([
        credentials(name: 'MAILCHIMP_API', credentialType: 'org.jenkinsci.plugins.plaincredentials.impl.StringCredentialsImpl', required: false)
    ])
])


node {

  def DRC_PATH="/root/jenkins/jenkins_home/workspace/backend"
  env.NODEJS_HOME = "${tool 'node'}"
  env.PATH="${env.NODEJS_HOME}/bin:${env.PATH}"
  currentBuild.result = 'SUCCESS'
  boolean skipBuild = false

  def MAILCHIMP_API = ''

  withCredentials([string(credentialsId:  '${MAILCHIMP_API}', variable: 'MAILCHIMP_API')]) {
    sh 'echo $MAILCHIMP_API'
    MAILCHIMP_API = sh 'echo $MAILCHIMP_API'

  }

  echo "${MAILCHIMP_API}"

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

    stage('Image Build'){
      imageBuild(CONTAINER_NAME, CONTAINER_TAG, DRC_PATH, branch, MAILCHIMP_API)
    }

    stage('Run App'){
      runApp(CONTAINER_NAME, CONTAINER_TAG, HTTP_PORT, DRC_PATH, branch, MAILCHIMP_API)
    }
  } catch (err) {
    currentBuild.result = 'FAILED'
    throw err
  }

}

def imagePrune(DRC_PATH, branch){
    try {
        sh "docker-compose -f docker-compose.${branch}.yml --project-directory=${DRC_PATH}_${branch} down -v"
        sh "docker-compose -f docker-compose.${branch}.yml --project-directory=${DRC_PATH}_${branch} rm -f --remove-orphans"
    } catch(error){}
}

def imageBuild(containerName, tag, DRC_PATH, branch, MAILCHIMP_API){
    sh "MAILCHIMP_API=${MAILCHIMP_API} docker-compose -f docker-compose.${branch}.yml --project-directory=${DRC_PATH}_${branch} build"
    echo "Image build complete"
}

def runApp(containerName, tag, httpPort, DRC_PATH, branch, MAILCHIMP_API){
    sh "MAILCHIMP_API=${MAILCHIMP_API} docker-compose -f docker-compose.${branch}.yml --project-directory=${DRC_PATH}_${branch} up -d --force-recreate"
    echo "Application started on port: ${httpPort} (http)"
}

