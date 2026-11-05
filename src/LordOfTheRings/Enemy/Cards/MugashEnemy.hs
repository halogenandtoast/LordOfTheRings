module LordOfTheRings.Enemy.Cards.MugashEnemy (mugashEnemy) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (engageCaptor)

newtype MugashEnemy = MugashEnemy EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mugashEnemy :: EnemyCard MugashEnemy
mugashEnemy = enemy MugashEnemy Cards.mugashEnemy

{- | Cannot have attachments. Defeated, he stays in play for his Forced effect to attach
him. As a Captive attachment: "Attached hero cannot ready."
-}
instance HasModifiersFor MugashEnemy where
  getModifiersFor (MugashEnemy a) = do
    modified a a [CannotHaveAttachments, StaysInPlayWhenDefeated]
    withHost a \host -> modified a host [CannotReady]

{- | Forced: After Mugash is defeated, the first player attaches him to a hero he controls
and exhausts that hero. Counts as a Captive attachment with the text: "Attached hero
cannot ready. If attached hero leaves play, the players lose the game."
Attached, he is no enemy: his damage is gone, and an attack on him or a Mugash's Guard
reading him finds him on the hero.
-}
instance RunMessage MugashEnemy where
  runMessage msg e@(MugashEnemy attrs) = case msg of
    CheckWindows ws | notNull [() | Window After (EnemyDefeated eid) <- ws, eid == attrs.id] -> do
      lead <- getLead
      heroes <- select (HeroControlledBy $ PlayerWithId lead)
      chooseOrRunOneM lead do
        questionLabeled "mugashEnemy.attach"
        causedBy attrs
        targets heroes \h -> do
          push $ DiscardShadowCards attrs.id
          placeEnemy attrs.id (AttachedTo $ toTarget h)
          exhaust h
          doStep 1 msg
      MugashEnemy <$> runMessage msg attrs
    -- the guards in play engage the player whose hero holds him
    DoStep 1 (CheckWindows _) -> do
      guards <- select (EnemyWithCardCode $ toCardCode Cards.mugashsGuard)
      traverse_ engageCaptor guards
      pure e
    PlaceEnemy eid (AttachedTo _) | eid == attrs.id -> do
      MugashEnemy <$> runMessage msg (attrs & tokensL .~ mempty)
    -- his hero left play, and he leaves with it
    Discard _ (EnemyTarget eid) | eid == attrs.id, AttachedTo _ <- attrs.placement -> do
      gameOver Lost
      pure e
    _ -> MugashEnemy <$> runMessage msg attrs
