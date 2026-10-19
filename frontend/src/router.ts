import { createRouter, createWebHashHistory } from 'vue-router'
import { currentUser } from './auth'
import Home from './views/Home.vue'

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', component: Home, meta: { title: 'appName', requiresAuth: true } },
    { path: '/new-game', component: () => import('./views/NewGame.vue'), meta: { title: 'newGame.title', requiresAuth: true } },
    { path: '/games/:id', component: () => import('./views/Game.vue'), meta: { title: 'common.game', requiresAuth: true } },
    { path: '/cards', component: () => import('./browser/views/BrowseView.vue'), meta: { title: 'nav.cards' } },
    { path: '/cards/:code', component: () => import('./browser/views/CardDetailView.vue'), meta: { title: 'titles.card' } },
    { path: '/decks', name: 'decks', component: () => import('./browser/views/DecksView.vue'), meta: { title: 'nav.decks', requiresAuth: true } },
    { path: '/decks/:id', name: 'deck', component: () => import('./browser/views/DeckDetailView.vue'), meta: { title: 'titles.deck', requiresAuth: true } },
    { path: '/settings', component: () => import('./views/Settings.vue'), meta: { title: 'settings.title' } },
    { path: '/sign-in', component: () => import('./views/SignIn.vue'), meta: { title: 'titles.signIn', guest: true } },
    { path: '/sign-up', component: () => import('./views/SignUp.vue'), meta: { title: 'titles.signUp', guest: true } },
  ],
})

router.beforeEach((to) => {
  if (to.meta.requiresAuth && !currentUser.value) return { path: '/sign-in', query: { next: to.fullPath } }
  if (to.meta.guest && currentUser.value) return { path: '/' }
})
