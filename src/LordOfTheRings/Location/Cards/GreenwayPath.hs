module LordOfTheRings.Location.Cards.GreenwayPath (greenwayPath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype GreenwayPath = GreenwayPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greenwayPath :: LocationCard GreenwayPath
greenwayPath = location GreenwayPath Cards.greenwayPath

-- | Forced: At the end of the round, if Greenway Path is in the staging area, search the
-- encounter deck, discard pile and victory display for Shadow the War Party, and reveal it.
instance HasAbilities GreenwayPath where
  getAbilities (GreenwayPath a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == StagingArea]

-- | Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage GreenwayPath where
  runMessage msg l@(GreenwayPath a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      searchAndRevealEncounterCard (cardIs Quests.shadowTheWarParty)
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> GreenwayPath <$> runMessage msg a
