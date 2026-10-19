import { createRouter, createWebHashHistory } from 'vue-router'
import Home from './views/Home.vue'

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', component: Home, meta: { title: 'The Lord of the Rings' } },
    { path: '/new-game', component: () => import('./views/NewGame.vue'), meta: { title: 'New Game' } },
    { path: '/games/:id', component: () => import('./views/Game.vue'), meta: { title: 'Game' } },
  ],
})

router.afterEach((to) => {
  document.title = (to.meta.title as string | undefined) ?? 'The Lord of the Rings'
})
