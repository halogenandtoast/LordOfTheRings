module LordOfTheRings.Treachery.Cards.HauntingFog (hauntingFog) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HauntingFog = HauntingFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hauntingFog :: TreacheryCard HauntingFog
hauntingFog = treachery HauntingFog Cards.hauntingFog

-- | Counts as a Condition attachment with the text: "Attached quest gets +6 quest points."
instance HasModifiersFor HauntingFog where
  getModifiersFor (HauntingFog a) = withHost a \host -> modified a host [QuestPointModifier 6]

-- | When Revealed: Each player must exhaust a character he controls. Attach Haunting Fog to
-- the current quest.
instance RunMessage HauntingFog where
  runMessage msg t@(HauntingFog attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> chooseAndExhaust attrs p 1
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    _ -> HauntingFog <$> runMessage msg attrs
