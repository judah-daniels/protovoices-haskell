build: 
		@docker build -t protoharmony .

run: build
    @docker run -it --shm-size=30g --cpus 10 --gpus all --name protoharmony protoharmony /bin/bash

clean: 
		-@docker rmi -f protoharmony &> /dev/null || true

rebuild: clean run
