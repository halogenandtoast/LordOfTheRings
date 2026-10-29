module LordOfTheRings.Treachery.Cards.DrivenBack (drivenBack) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DrivenBack = DrivenBack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

drivenBack :: TreacheryCard DrivenBack
drivenBack = treachery DrivenBack Cards.drivenBack

-- | When Revealed: Remove all progress from each location in the staging area. Increase
-- the total threat in the staging area by 1 for each progress removed this way. If no
-- progress was removed by this effect, Driven Back gains surge.
-- The card names no duration: the increase counts toward the quest it is revealed for, so
-- it lasts until the end of the phase.
instance RunMessage DrivenBack where
  runMessage msg t@(DrivenBack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      staged <- select LocationInStagingArea
      locations <- filterM (fmap (notElem ImmuneToCardEffects) . getModifiers) staged
      progress <- for locations \lid -> do
        n <- (.progress) <$> getLocation lid
        when (n > 0) $ removeTokens attrs lid Progress n
        pure n
      if sum progress > 0
        then phaseModifier attrs GameTarget (StagingThreatBonus $ sum progress)
        else revealEncounterCard
      pure t
    _ -> DrivenBack <$> runMessage msg attrs
