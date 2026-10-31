module LordOfTheRings.Treachery.Cards.Isolation (isolation) where

import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Isolation = Isolation TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

isolation :: TreacheryCard Isolation
isolation = treachery Isolation Cards.isolation

-- | Counts as a Condition attachment with the text: "All characters lose the ranged and
-- sentinel keywords. The player guarding Gollum cannot choose a new player to guard
-- Gollum." (Return to Mirkwood's 'passGollum' reads the second.)
instance HasModifiersFor Isolation where
  getModifiersFor (Isolation a) = case a.placement of
    AttachedTo _ -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> modified a c [LoseKeyword Ranged, LoseKeyword Sentinel]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach Isolation to the current quest.
instance RunMessage Isolation where
  runMessage msg t@(Isolation attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getCurrentQuest >>= traverse_ (attachTreachery attrs.treacheryId)
      pure t
    _ -> Isolation <$> runMessage msg attrs
