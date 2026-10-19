import { createRouter, createWebHashHistory } from 'vue-router'
import { currentUser } from './auth'
import Home from './views/Home.vue'

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', component: Home, meta: { title: 'The Lord of the Rings', requiresAuth: true } },
    { path: '/new-game', component: () => import('./views/NewGame.vue'), meta: { title: 'New Game', requiresAuth: true } },
    { path: '/games/:id', component: () => import('./views/Game.vue'), meta: { title: 'Game', requiresAuth: true } },
    { path: '/sign-in', component: () => import('./views/SignIn.vue'), meta: { title: 'Sign In', guest: true } },
    { path: '/sign-up', component: () => import('./views/SignUp.vue'), meta: { title: 'Sign Up', guest: true } },
  ],
})

router.beforeEach((to) => {
  if (to.meta.requiresAuth && !currentUser.value) return { path: '/sign-in', query: { next: to.fullPath } }
  if (to.meta.guest && currentUser.value) return { path: '/' }
})

router.afterEach((to) => {
  document.title = (to.meta.title as string | undefined) ?? 'The Lord of the Rings'
})
