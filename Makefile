include .env

reset-cache-resource-only:
	- docker-compose kill cache resource cache-warmup
	- docker-compose up -d cache resource cache-warmup

reset-cache:
	- docker-compose kill yggdrasil triplestore file cache resource migrations cache-warmup publication-report
	- rm -rf ./testdata/db && rm -rf ./testdata/files
	- unzip -o ./testdata.zip -d .
	- docker-compose up -d

reset-elastic-and-cache:
	- docker-compose kill yggdrasil triplestore elasticsearch search file file-bundling docx-conversion cache resource migrations cache-warmup publication-report
	- docker-compose rm -f yggdrasil triplestore elasticsearch search file file-bundling docx-conversion cache resource migrations cache-warmup publication-report
	- rm -rf ./testdata
	- rm -rf ./testdata-elasticsearch
	- unzip -o ./testdata.zip -d .
	- unzip -o ./testdata-elasticsearch.zip -d .
	- mv ./testdata-elasticsearch/* ./testdata
	- rm -rf ./testdata-elasticsearch
	- docker-compose up -d

me-a-sandwich:
	@echo making you a choco sandwich

drc-up-d:
	- docker-compose up -d

drc-kill:
	- docker-compose kill && docker-compose rm -f

drc:
	- docker-compose ${args}
