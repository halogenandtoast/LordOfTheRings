module LordOfTheRings.Treachery.Cards.WildMenOfDunland (wildMenOfDunland) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WildMenOfDunland = WildMenOfDunland TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wildMenOfDunland :: TreacheryCard WildMenOfDunland
wildMenOfDunland = treachery WildMenOfDunland Cards.wildMenOfDunland

-- | Counts as a Condition attachment with the text: "Forced: After a player draws any number
-- of cards, that player deals 1 damage to a character he controls."
instance HasAbilities WildMenOfDunland where
  getAbilities (WildMenOfDunland a) = [mkAbility a 1 $ forced $ DrewCardsWindow After Nothing | AttachedTo _ <- [a.placement]]

-- | Surge.
-- When Revealed: Attach to the current quest.
instance RunMessage WildMenOfDunland where
  runMessage msg t@(WildMenOfDunland attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (CardsDrawn pid _) <- windows] \pid -> dealDamageToOneCharacter attrs pid 1
      pure t
    _ -> WildMenOfDunland <$> runMessage msg attrs
