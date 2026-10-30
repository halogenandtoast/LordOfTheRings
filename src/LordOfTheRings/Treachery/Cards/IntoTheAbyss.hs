module LordOfTheRings.Treachery.Cards.IntoTheAbyss (intoTheAbyss) where

import LordOfTheRings.Card.CardType (CardType (..))
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)
import LordOfTheRings.Trait (Trait (Creature))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype IntoTheAbyss = IntoTheAbyss TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoTheAbyss :: TreacheryCard IntoTheAbyss
intoTheAbyss = treachery IntoTheAbyss Cards.intoTheAbyss

{- | When Revealed: Each player chooses a card type and discards each card in his hand that
is not of the chosen card type. If no cards are discarded by this effect, Into the Abyss
gains surge.
Shadow: If the active location is Underwater, search the encounter deck and discard pile
for a Creature enemy, and add it to the staging area.

A player chooses among the types in his hand: one with a single type keeps it all, so
nothing is discarded unless some hand holds two types.
-}
instance RunMessage IntoTheAbyss where
  runMessage msg t@(IntoTheAbyss attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      hands <- for players \p -> (p,) . (.hand) <$> getPlayer p
      let typesIn = ordNub . map (.cardType)
      for_ hands \(p, hand) -> when (length (typesIn hand) > 1) $ chooseOneM p do
        questionLabeled "intoTheAbyss.choose"
        causedBy attrs
        for_ (typesIn hand) \ty -> labeled ("intoTheAbyss." <> typeKey ty) do
          traverse_ (discardFromHand p) (filter ((/= ty) . (.cardType)) hand)
      when (all ((<= 1) . length . typesIn . snd) hands) revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      whenM underwaterActive $ searchEncounterDeckAndDiscard pid attrs (CardWithType EnemyType <> CardWithTrait Creature)
      pure t
    _ -> IntoTheAbyss <$> runMessage msg attrs
   where
    typeKey = \case
      AllyType -> "ally"
      AttachmentType -> "attachment"
      EventType -> "event"
      PlayerSideQuestType -> "sideQuest"
      _ -> "other"
