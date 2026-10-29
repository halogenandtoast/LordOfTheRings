module LordOfTheRings.Objective.Cards.UpInFlames (upInFlames) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (Unplaced))

newtype UpInFlames = UpInFlames ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

upInFlames :: ObjectiveCard UpInFlames
upInFlames = objective UpInFlames Cards.upInFlames

-- | Forced: At the end of the round, place 1 resource token on Up in Flames and discard the
-- top X cards of each player's deck. X is the number of resource tokens on Up in Flames.
instance HasAbilities UpInFlames where
  getAbilities (UpInFlames a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | If any player has no cards in his deck, the players have lost the game.
-- Checked as cards leave a deck by drawing, discarding (the Forced effect too) or being
-- taken, and as each window opens, so a deck already empty as it enters play is caught as
-- it enters the staging area.
instance RunMessage UpInFlames where
  runMessage msg o@(UpInFlames attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      let x = countTokens Resource attrs.objectiveTokens + 1
      placeResources attrs attrs 1
      eachPlayer \p -> discardTopOfDeck p x
      pure o
    -- read once the players have taken the cards
    DoStep 1 (deckChange -> True) -> do
      players <- traverse getPlayer =<< getRemainingPlayers
      when (any (null . (.deck)) players) $ gameOver Lost
      pure o
    (deckChange -> True) -> do
      doStep 1 msg
      UpInFlames <$> runMessage msg attrs
    CheckWindows _ | attrs.placement /= Unplaced -> do
      players <- traverse getPlayer =<< getRemainingPlayers
      when (any (null . (.deck)) players) $ gameOver Lost
      UpInFlames <$> runMessage msg attrs
    _ -> UpInFlames <$> runMessage msg attrs
   where
    deckChange = \case
      DrawCards _ -> True
      DiscardTopOfDeck _ _ -> True
      TakeFromDeck _ _ -> True
      RemoveFromDeck _ _ -> True
      _ -> False
