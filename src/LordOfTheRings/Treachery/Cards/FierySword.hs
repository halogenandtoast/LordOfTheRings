module LordOfTheRings.Treachery.Cards.FierySword (fierySword) where

import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FierySword = FierySword TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fierySword :: TreacheryCard FierySword
fierySword = treachery FierySword Cards.fierySword

-- | Counts as a Weapon attachment with the text: "Attached enemy gets +3 attack."
instance HasModifiersFor FierySword where
  getModifiersFor (FierySword a) = withHost a \host -> modified a host [AttackModifier 3]

-- | When Revealed: Attach Fiery Sword to Durin's Bane.
instance RunMessage FierySword where
  runMessage msg t@(FierySword attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      attachToDurinsBane attrs
      pure t
    _ -> FierySword <$> runMessage msg attrs
