module LordOfTheRings.Location.Cards.AFloodedFord (aFloodedFord) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype AFloodedFord = AFloodedFord LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aFloodedFord :: LocationCard AFloodedFord
aFloodedFord = location AFloodedFord Cards.aFloodedFord

-- | Each card revealed by the encounter deck gains doomed X. X is the number of progress
-- tokens on this card.
instance HasAbilities AFloodedFord where
  getAbilities (AFloodedFord a) = [mkAbility a 1 $ silent $ EncounterCardRevealedWindow After | a.progress > 0]

-- | Shadow: If this attack is undefended, put A Flooded Ford into the staging area with 1
-- progress token on it.
instance RunMessage AFloodedFord where
  runMessage msg l@(AFloodedFord a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      raiseThreatFromDoomed a.progress
      pure l
    ResolveShadowEffect cid eid _ defender | cid == a.cardId && null defender -> do
      removeShadowCard eid cid
      putLocationIntoPlay (MkEncounterCard cid (toCardCode a)) StagingArea
      -- the copy now in play takes the progress, the card id its own
      doStep 2 msg
      pure l
    DoStep 2 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      placeProgress a a 1
      pure l
    _ -> AFloodedFord <$> runMessage msg a
