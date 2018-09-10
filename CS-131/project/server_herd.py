import sys
import asyncio
import time
import json
from aiohttp import ClientSession
#from async_fetch import get_json

server_names = ["Goloman", "Hands", "Holiday", "Welsh", "Wilkes"]
debug_flag = True

port = {
	"Goloman":16670,
	"Hands":16671,
	"Holiday":16672,
	"Welsh":16673,
	"Wilkes":16674
}

relation = {
	"Goloman":["Hands", "Holiday", "Wilkes"],
	"Hands":["Wilkes","Goloman"],
	"Holiday":["Welsh", "Wilkes", "Goloman"],
	"Welsh":["Holiday"],
	"Wilkes":["Goloman", "Hands", "Holiday"]
}

clients={}

class Inter_server_connection(asyncio.Protocol):
	def __init__(self, message):
		super().__init__()
		self.message = message

	def connection_made(self, transport):
		self.transport = transport
		self.peername = transport.get_extra_info('peername')
		transport.write(self.message.encode())
		f.write("Connected to " + str(self.peername) +'\n')
		if debug_flag:
			print ("Connection to " + str(self.peername))

	def connection_lost(self, exc):
		self.peername = self.transport.get_extra_info('peername')
		f.write("Disconnected from " + str(self.peername) + '\n')
		if debug_flag:
			print ("Connection lost from " + str(self.peername))


	def data_received(self, data):
		self.peername = self.transport.get_extra_info('peername')
		f.write("Data received from " + str(self.peername) + '\n')
		if debug_flag:
			print ("Data received from " + str(self.peername))

# class Google_place_connection(asyncio.protocol):

class Server(asyncio.Protocol):
	def __init__(self, name):
		super().__init__()
		self.name = name

	def connection_made(self, transport):
		self.transport = transport
		self.peername = transport.get_extra_info('peername')
		f.write("Connected to " + str(self.peername) +'\n')
		if debug_flag:
			print ("Connection from " + str(self.name) + " to " + str(self.peername))

	def connection_lost(self, exc):
		self.peername = self.transport.get_extra_info('peername')
		f.write("Disconnected from " + str(self.peername) + '\n')
		if debug_flag:
			print ("Connection lost between " + str(self.name) + " to " + str(self.peername))

	def data_received(self, data):
		if debug_flag:
			print("received data")
		self.peername = self.transport.get_extra_info('peername')
		f.write("Data received from " + str(self.peername) + '\n')
		message = data.decode()
		message_seg = message.split(" ")
		if len(message_seg) < 1:
			self.except_invalid_msg(message)
			return
		else:
			if message_seg[0] == "IAMAT":
				self.processIAMAT(message)
			elif message_seg[0] == "AT":
				self.processAT(message)
			elif message_seg[0] == "WHATSAT":
				asyncio.Task(self.processWHATSAT(message))
			else:
				self.except_invalid_msg(message)
				return

	def except_invalid_msg(self, message):
		self.transport.write( ("? " + message).encode() )
		self.peername = self.transport.get_extra_info('peername')
		f.write("Illegal Command from " + str(self.name) + " to " + str(self.peername) + " " + str(message) + "\n")
		self.transport.close()


	def propogate(self, srv_name, port_num, message):
		if debug_flag:
			print("propogating between servers")

		coroutine = event_loop.create_connection(lambda: Inter_server_connection(message), '127.0.0.1', port_num)
		f.write("Connection from " + str(self.name) + " to " + str(srv_name) + ": " + str(message) +'\n')
		if debug_flag:
			print ("Connection from " + str(self.name) + " to " + str(srv_name) + ": " + str(message))
		event_loop.create_task(coroutine)
		

	async def Google_place_connection(self, url):
		async with ClientSession() as session:
			async with session.get(url) as response:
				response = await response.read()
				return response

	def parsing_json (self, output, client_addr, upper_bound):
		data = json.loads(output)
		data['results'] = data['results'][0:int(upper_bound)]
		json_str = json.dumps(data, indent=3)
		issue_time = clients[client_addr][1][-1]
		coordinate = clients[client_addr][1][4]
		response = clients[client_addr][0] + '\n'
		response += json_str
		response = response.rstrip() + '\n\n'
		self.transport.write(response.encode())
		self.transport.close()
		f.write("Response Sent: " + str(response) + '\n')
		if debug_flag:
			print("Response Sent: " + str(response))

	def processIAMAT(self, message):
		if debug_flag:
			print("Processing IAMAT")
		f.write("Received Message: " + message + "\n")
		message_seg = message.split(" ")
		if len(message_seg) != 4:
			self.except_invalid_msg(message)
			if debug_flag:
				print ("IAMAT: Wrong Number of Parameters in Command.")
		
		#parsing IAMAT message
		receive_time = time.time()
		client_addr = message_seg[1]
		coordinate = message_seg[2]
		issue_time = float(message_seg[3])
		time_ret = receive_time - issue_time
		mid = int(len(coordinate)/2)
		coordinate_x = coordinate[0:mid]
		coordinate_y = coordinate[mid:]

		if issue_time < 0.0:
			self.except_invalid_msg(message)
			if debug_flag:
				print ("IAMAT: Illegal Time")

		#generate respose and share between servers
		if (client_addr in clients) and (issue_time < float(clients[client_addr][1][5])):
			response = clients[client_addr][0]
			self.transport.write(response.encode())
			self.transport.close()

		else:
			response = "AT "
			response += str(self.name)
			response += " "
			response += str(time_ret)
			response += " "
			response += str(client_addr)
			response += " "
			response += str(coordinate)
			response += " "
			response += message_seg[3]
			self.transport.write(response.encode())
			self.transport.close()
			self.processAT(response)
		f.write("Generated Message: " + response + "\n")


	def processAT(self, message):
		#checking AT message
		if debug_flag:
			print("Processing AT")
		self.transport.close()
		message_seg = message.split(" ")
		
		if len(message_seg) != 6:
			self.except_invalid_msg(message)
			if debug_flag:
				print ("AT: Wrong Number of Parameters in Command.")

		#parsing AT message - not necesarry
		client_addr = message_seg[3]
		issue_time = float(message_seg[5])

		if issue_time < 0:
			self.except_invalid_msg(message)
			if debug_flag:
				print ("AT: Illegal Time")
		#update clients

		if (client_addr in clients) and (issue_time <= float(clients[client_addr][1][5])):
			if debug_flag:
				print ("client cached")
			return
		else:
			if debug_flag:
				print ("cache missed")
			clients[client_addr] = []
			clients[client_addr].append(str(message))
			clients[client_addr].append(message_seg)
			for srv in relation[self.name]:
				port_num = port[srv]
				self.propogate(srv, port_num, message)

	@asyncio.coroutine
	def processWHATSAT(self, message):
		message_seg = message.split(" ")
		
		if len(message_seg) != 4:
			self.except_invalid_msg(message)
			if debug_flag:
				print ("WHATSAT: Wrong Number of Parameters in Command.")

		#parsing AT message - not necesarry
		client_addr = message_seg[1]
		radius = message_seg[2]
		upper_bound = message_seg[3]
		if int(radius) > 50 or int(upper_bound) > 20:
			self.except_invalid_msg(message)
			if debug_flag:
				print ("WHATSAT: Wrong Number of Parameters in Command.")
		
		if client_addr not in clients:
			f.write("WHATSAT: " + str(client_addr) + " not found!\n")
			raise Exception(str(client_addr) + " not found!")
			return

		if debug_flag:
			print (clients[client_addr])

		coordinate = clients[client_addr][1][4]
		mid = int(len(coordinate)/2)
		coordinate_x = coordinate[0:mid]
		coordinate_y = coordinate[mid:]
		coordinate2 = coordinate_x.strip('+') + ',' + coordinate_y.strip('-')

		url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
		url += "location=" + str(coordinate2)
		url += "&radius=" + str(radius)
		url += "&key=AIzaSyDrz-2e9YOKXDCohZ6SDKHsa4tYGrZe2y4"

		# output = get_json(url)
		# self.parsing_json(output, client_addr, upper_bound)
		output = yield from self.Google_place_connection(url)
		self.parsing_json(output, client_addr, upper_bound)


	#def parsing_json

server_name = sys.argv[1]
if not server_name in server_names:
	raise Exception("Error: Invalid Server Name")
log_file = server_name + ".txt"
with open(log_file, 'w') as f:
	# create a event_loop
	event_loop = asyncio.get_event_loop()
	port_num = port[server_name]

	# create a server
	coroutine = event_loop.create_server(lambda: Server(server_name), '127.0.0.1', port_num)
	server = event_loop.run_until_complete(coroutine)
	# write in the log
	f.write("Server Running: " + server_name + '\n')
	try:
		event_loop.run_forever()
	except KeyboardInterrupt:
		pass

	server.close()
	event_loop.run_until_complete(server.wait_closed())

	event_loop.close()
	
	

