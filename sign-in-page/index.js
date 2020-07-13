const Vue = require('vue')
const {
  focus
} = require('vue-focus')

const element = document.createElement('div')
element.id = 'app'
element.innerHTML = '<app></app>'
document.body.appendChild(element)

new Vue({
  el: '#app',
  components: {
    app: {
      data: () => ({
        locked: false,
        incorrectCredentials: false,
        userNameFocused: true,
        password: '',
        userName: ''
      }),
      directives: {
        focus: focus
      },
      methods: {
        onSubmit() {
          this.locked = true

          fetch('/api/login', {
              body: JSON.stringify({
                user: this.userName,
                password: this.password
              }),
              headers: {
                'Content-Type': 'application/json'
              },
              method: 'POST'
            })
            .then(response => {
              if (response.status === 403) {
                this.incorrectCredentials = true
                this.locked = false
              } else {
                location.reload()
              }
            })
        }
      },
      template: `
        <section>
          <h1>Logga in</h1>
          <form @submit.prevent="onSubmit()">
            <label>Användarnamn <input placeholder="Användarnamn" v-focus="userNameFocused" v-model="userName" @blur="userNameFocused = false" @focus="userNameFocused = true"></label>
            <label>Lösenord <input placeholder="Lösenord" type="password" v-model="password"></label>
            <input v-bind:disabled="locked" type="submit" value="Logga in">
            <span v-if="incorrectCredentials">Felaktigt användarnamn eller lösenord.</span>
          </form>
        </section>
      `
    }
  }
})
