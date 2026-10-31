module LordOfTheRings.Location.Cards.BramblesAndThorns (bramblesAndThorns) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype BramblesAndThorns = BramblesAndThorns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bramblesAndThorns :: LocationCard BramblesAndThorns
bramblesAndThorns = location BramblesAndThorns Cards.bramblesAndThorns

instance HasAbilities BramblesAndThorns where
  getAbilities (BramblesAndThorns a) = [mkAbility a 1 $ forced $ AnyCommittedToQuestWindow After]

-- | Forced: After a character commits to the quest, deal 1 damage to that character if it
-- has not been dealt damage by another copy of Brambles and Thorns this round.
-- Every copy marks the characters it damaged with an effect from the card's code.
instance RunMessage BramblesAndThorns where
  runMessage msg l@(BramblesAndThorns a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      let marker = CardCodeSource (toCardCode a)
      marked <- getsGame (map effectTarget . filter ((== marker) . effectSource) . gameEffects)
      for_ [c | Window _ (CommittedToQuest c) <- windows, c `notElem` marked] \c -> do
        dealDamage a c 1
        roundModifier marker c DealtDamageThisRound
      pure l
    _ -> BramblesAndThorns <$> runMessage msg a
