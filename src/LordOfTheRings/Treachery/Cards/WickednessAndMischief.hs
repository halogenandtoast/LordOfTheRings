module LordOfTheRings.Treachery.Cards.WickednessAndMischief (wickednessAndMischief) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (tantrumTargets)
import LordOfTheRings.Trait (Trait (Tantrum))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WickednessAndMischief = WickednessAndMischief TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wickednessAndMischief :: TreacheryCard WickednessAndMischief
wickednessAndMischief = treachery WickednessAndMischief Cards.wickednessAndMischief

-- | When Revealed: The player guarding Gollum must either reveal the topmost Tantrum card
-- in the encounter discard pile, or reveal 2 additional encounter cards from the encounter
-- deck.
-- Shadow: Deal the topmost Tantrum card in the encounter discard pile as a new shadow card
-- for this attack, if able.
instance RunMessage WickednessAndMischief where
  runMessage msg t@(WickednessAndMischief attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      tantrum <- topTantrum
      players <- tantrumTargets
      for_ players \p -> chooseOneM p do
        questionLabeled "wickednessAndMischief.choose"
        causedBy attrs
        for_ tantrum \c -> labeled "wickednessAndMischief.tantrum" $ revealFromEncounterDiscard c
        labeled "wickednessAndMischief.reveal" $ replicateM_ 2 revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      traverse_ (dealAndResolveShadowCard eid pid defenders) =<< topTantrum
      pure t
    _ -> WickednessAndMischief <$> runMessage msg attrs
   where
    topTantrum = find ((Tantrum `member`) . cdCardTraits . toCardDef) <$> getEncounterDiscard
