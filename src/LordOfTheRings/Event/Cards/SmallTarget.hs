module LordOfTheRings.Event.Cards.SmallTarget (smallTarget) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyShadowRevealed))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense)
import LordOfTheRings.Trait (Trait (Hobbit))

newtype SmallTarget = SmallTarget EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a Hobbit hero you control exhausts to defend an attack, choose another
-- enemy engaged with you and reveal the attacking enemy's shadow card. If that shadow card
-- has no shadow effect, resolve this enemy's attack against the chosen enemy. If that
-- shadow card has a shadow effect, resolve this attack as normal.
smallTarget :: EventCard SmallTarget
smallTarget =
  eventWith SmallTarget Cards.smallTarget \a ->
    a
      & windowL ?~ HeroDefenderDeclaredWindow After (HeroWithTrait Hobbit <> HeroControlledBy (PlayerWithId a.owner) <> ExhaustedHero)
      -- ponytail: the attacker is taken to be engaged with you, as every attack in the
      -- combat phase is; an immediate attack from the staging area may leave the second
      -- enemy this asks for unchecked
      & criteriaL .~ EnemiesAtLeast 2 (EnemyEngagedWith $ PlayerWithId a.owner)

instance RunMessage SmallTarget where
  runMessage msg e@(SmallTarget attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (headMay [(d, eid) | Window _ (DeclaredAsDefender d eid) <- windows]) \(defender, eid) -> do
        others <- select (EnemyEngagedWith (PlayerWithId pid) <> not_ (EnemyWithId eid))
        enemy <- getEnemy eid
        let facedown = [c | c <- enemy.shadowCards, c.id `notElem` enemyShadowRevealed (toAttrs enemy)]
        chooseOneM pid do
          questionLabeled "smallTarget.chooseEnemy"
          causedBy attrs
          targets others \other ->
            -- ponytail: an enemy dealt no shadow card has none with a shadow effect
            -- one with an effect is left to the shadow step, which turns it over and
            -- resolves it, so the attack resolves as normal
            unless (any (cdShadowEffect . toCardDef) facedown) do
                for_ facedown \c -> push $ RevealShadowCard eid c.id
                -- ponytail: the damage is reckoned as Small Target resolves, not at the
                -- attack's damage step
                damage <- (-) <$> getEnemyAttack eid <*> getEnemyDefense other
                cancelMatching (== AttackDamage eid defender)
                dealDamage (EnemySource eid) other (max 0 damage)
      pure e
    _ -> SmallTarget <$> runMessage msg attrs
