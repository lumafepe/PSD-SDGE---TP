import yaml
import argparse
import io
import os

try:
    os.mkdir("configFiles")
except:
    pass

getConfig = lambda x : {
    "dht":{
        "address": f"dhtnode{x}",
        "port": 4200,
        "baseDirectory": "/tmp/dht/",
        "tokenCount": 20,
        "mod": 4294967296
    },
    "server":{
        "address": "central_server",
        "port": 4321   
    }
}



data = {
    'services': {
        'central_server': {
            'build': {
                'context': './server', 
                'dockerfile': 'Dockerfile'
            },
            'stdin_open': True, 
            'tty': True,
            'networks': [
                'lost'
            ],
            'ports': [
                '4321:4321'
            ]
        }
    },
    'networks':{
        'lost':{
            'driver': 'bridge'
        }
    }
}


parser = argparse.ArgumentParser(
                    prog='docker-compose-generator',
                    description='generates a docker-compose with custom names',
                    epilog='generator')


parser.add_argument('-c', '--clients',choices=range(0, 999),type=int,help='number of clients to create')
parser.add_argument('-d', '--dht',choices=range(0, 999),type=int,help='number of dht nodes to create',default=1)

args = parser.parse_args()

genNode = lambda x: (f'dhtnode{x}',{
            'build': {
                'context': './DHT', 
                'dockerfile': 'Dockerfile'
            },
            'volumes': [
                f'./configFiles/config_{x}.yml:/config.yml',
                f'./dhtData/dhtnode{x}:/tmp/dht/'
            ],
            'networks': [
                'lost'
            ],
            'depends_on': ['central_server'] + [f'dhtnode{i}' for i in range(x)]
        })

genClient = lambda x: (f'client{x}',{
    'build': {
                'context': './client', 
                'dockerfile': 'Dockerfile'
            },
    'networks': [ 'lost'],
    'depends_on': ['central_server'],
    'environment': [
        f"ID={x}",
        f"IP=central_server"
    ]
})


for i in range(args.dht):
    name,d = genNode(i)
    config = getConfig(i)
    with io.open(f'./configFiles/config_{i}.yml', 'w', encoding='utf8') as outfile:
        yaml.dump(config, outfile, default_flow_style=False, allow_unicode=True,indent=2)
    data['services'][name] = d
    
for i in range(args.clients):
    name,d = genClient(i)
    data['services'][name] = d

# Write YAML file
with io.open('docker-compose.yml', 'w', encoding='utf8') as outfile:
    yaml.dump(data, outfile, default_flow_style=False, allow_unicode=True,indent=2)