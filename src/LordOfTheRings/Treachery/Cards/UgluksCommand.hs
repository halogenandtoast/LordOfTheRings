module LordOfTheRings.Treachery.Cards.UgluksCommand (ugluksCommand) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getPursuit, increasePursuit)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UgluksCommand = UgluksCommand TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ugluksCommand :: TreacheryCard UgluksCommand
ugluksCommand = treachery UgluksCommand Cards.ugluksCommand

instance RunMessage UgluksCommand where
  runMessage msg t@(UgluksCommand a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      ugluks <- filterM (canAttack you) =<< select (EnemyWithCardCode $ toCardCode Enemies.ugluk)
      chooseOrRunOneM you do
        questionLabeled "ugluksCommand.choose"
        causedBy a
        labeled "ugluksCommand.pursuit" $ increasePursuit 4
        when (notNull ugluks) $ labeled "ugluksCommand.attack" $ for_ ugluks (immediateAttack you)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      pursuit <- getPursuit
      enemyGetsAttack a eid ((pursuit `div` 10) `mod` 10)
      pure t
    _ -> UgluksCommand <$> runMessage msg a
   where
    canAttack pid eid = do
      mods <- getModifiers eid
      pure $ CannotAttack `notElem` mods && CannotAttackPlayer pid `notElem` mods
