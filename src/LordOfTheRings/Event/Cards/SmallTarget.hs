module LordOfTheRings.Event.Cards.SmallTarget (smallTarget) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyShadowRevealed))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
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
      & windowL
      ?~ HeroDefenderDeclaredWindow
        After
        (HeroWithTrait Hobbit <> HeroControlledBy (PlayerWithId a.owner) <> ExhaustedHero)
        (EnemyEngagedWith $ PlayerWithId a.owner)

-- | Of several shadow cards, the player reveals the one he chooses; the others resolve as
-- the attack's do (FAQ 3.174). An enemy dealt none has no shadow card to reveal, and its
-- attack resolves as normal. One with a shadow effect is left to the shadow step, which
-- turns it over and resolves it.
instance RunMessage SmallTarget where
  runMessage msg e@(SmallTarget attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (headMay [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid -> do
        others <- select (EnemyEngagedWith (PlayerWithId pid) <> not_ (EnemyWithId eid))
        enemy <- getEnemy eid
        let facedown = [c | c <- enemy.shadowCards, c.id `notElem` enemyShadowRevealed (toAttrs enemy)]
        chooseOneM pid do
          questionLabeled "smallTarget.chooseEnemy"
          causedBy attrs
          targets others \other -> chooseOrRunOneM pid do
            questionLabeled "smallTarget.chooseShadow"
            causedBy attrs
            targets facedown \c -> unless (cdShadowEffect $ toCardDef c) do
              push $ RevealShadowCard eid c.id
              redirectAttack eid other
      pure e
    _ -> SmallTarget <$> runMessage msg attrs
