import { createApp } from 'vue'
import mitt from 'mitt'
import './style.css'
import App from './App.vue'

const emitter = mitt()
const app = createApp(App)

app.config.globalProperties.emitter = emitter
app.provide('emitter', emitter)
app.mount('#app')
