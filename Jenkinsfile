def CONTAINER_NAME=""
def CONTAINER_TAG="latest"
def HTTP_PORT="8081"


node {

  def DRC_PATH="/root/jenkins_home/workspace/be-kaleidos"
  env.NODEJS_HOME = "${tool 'node'}"
  env.PATH="${env.NODEJS_HOME}/bin:${env.PATH}"
  currentBuild.result = 'SUCCESS'
  boolean skipBuild = false

  stage('Initialize'){
    def dockerHome = tool 'myDocker'
  }

  stage('Checkout') {
    checkout scm
  }

  try {

    stage("Image Prune"){
      imagePrune(CONTAINER_NAME, DRC_PATH)
    }

    stage('Image Build'){
      imageBuild(CONTAINER_NAME, CONTAINER_TAG, DRC_PATH)
    }

    stage('Run App'){
      runApp(CONTAINER_NAME, CONTAINER_TAG, HTTP_PORT, DRC_PATH)
    }
  } catch (err) {
    currentBuild.result = 'FAILED'
    throw err
  }

}

def imagePrune(containerName, DRC_PATH){
    try {
        sh "docker-compose --project-directory=${DRC_PATH} down -v"
        sh "docker-compose --project-directory=${DRC_PATH} rm -f"
    } catch(error){}
}

def imageBuild(containerName, tag, DRC_PATH){
    sh "docker-compose --project-directory=${DRC_PATH} build"
    echo "Image build complete"
}

def runApp(containerName, tag, httpPort, DRC_PATH){
    sh "docker-compose --project-directory=${DRC_PATH} up --build -d"
    echo "Application started on port: ${httpPort} (http)"
}

