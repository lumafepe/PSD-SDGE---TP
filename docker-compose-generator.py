import yaml
import argparse
import io

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
                    epilog='Perdeste o jogo')


#parser.add_argument('-c', '--clients',choices=range(1, 999),type=int,help='number of clients to create')
parser.add_argument('-d', '--dht',choices=range(1, 999),type=int,help='number of dht nodes to create',default=1)

args = parser.parse_args()

genNode = lambda x: (f'dht_node_{x}',{
            'build': {
                'context': './DHT', 
                'dockerfile': 'Dockerfile'
            },
            'networks': [
                'lost'
            ]
        })


for i in range(args.dht):
    name,d = genNode(i)
    data['services'][name] = d

# Write YAML file
with io.open('docker-compose.yml', 'w', encoding='utf8') as outfile:
    yaml.dump(data, outfile, default_flow_style=False, allow_unicode=True,indent=2)