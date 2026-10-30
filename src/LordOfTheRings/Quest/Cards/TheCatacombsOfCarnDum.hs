module LordOfTheRings.Quest.Cards.TheCatacombsOfCarnDum (theCatacombsOfCarnDum) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (reanimateTopOfDeck)
import LordOfTheRings.Trait (Trait (Sorcery))

newtype TheCatacombsOfCarnDum = TheCatacombsOfCarnDum QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCatacombsOfCarnDum :: QuestCard TheCatacombsOfCarnDum
theCatacombsOfCarnDum = quest TheCatacombsOfCarnDum Cards.theCatacombsOfCarnDum

{- | Setup: Set Daechanar and Altar of Midwinter aside, out of play (the scenario did). Each
player reveals an encounter card from the top of the encounter deck. Each player
reanimates the top card of his deck.
When the players advance to stage 2, all Sorcery cards attached to this stage are attached
to stage 2B: as the last progress lands they are held off the stage, so its defeat does
not discard them, and stage 2B takes them as it turns over.
-}
instance RunMessage TheCatacombsOfCarnDum where
  runMessage msg q@(TheCatacombsOfCarnDum attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      eachPlayer \_ -> revealEncounterCard
      doStep 1 msg
      pure $ TheCatacombsOfCarnDum $ attrs {questFlipped = True}
    -- the top card as it is once the reveals have resolved
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      eachPlayer \pid -> reanimateTopOfDeck pid 1
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.questPoints > 0, attrs.progress + n >= attrs.questPoints -> do
      sorcery <- getsGame \g ->
        [t.id | t <- toList g.entities.treacheries, t.placement == AttachedTo (toTarget attrs), Sorcery `member` (toCardDef t).traits]
      for_ sorcery \tid -> attachTreachery tid GameTarget
      TheCatacombsOfCarnDum <$> runMessage msg attrs
    _ -> TheCatacombsOfCarnDum <$> runMessage msg attrs
