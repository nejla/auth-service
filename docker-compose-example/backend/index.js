const http = require('http')

const app = http.createServer((_request, response) => {
  response.setHeader('Content-Type', 'application/json')
  response.write(JSON.stringify({ message: 'Hello World!' }))
  response.end()
})

app.listen({ port: 80 }, () => {
  console.log('The backend is running!')
})
