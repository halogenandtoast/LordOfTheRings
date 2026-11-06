module LordOfTheRings.Objective.Cards.RangerOfIthilien (rangerOfIthilien) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Helpers.Window (exhaustableCharacters)
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype RangerOfIthilien = RangerOfIthilien ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rangerOfIthilien :: ObjectiveCard RangerOfIthilien
rangerOfIthilien = objective RangerOfIthilien Cards.rangerOfIthilien

instance RunMessage RangerOfIthilien where
  runMessage msg o@(RangerOfIthilien attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      push $ PutObjectiveAllyIntoPlay attrs.id lead
      -- The encounter card keeps its id as it becomes a controlled ally. This
      -- enters committed, so it does not pay a cost for declaring a commitment.
      commitToQuest (CardIdTarget attrs.cardId)
      phaseModifier attrs (CardIdTarget attrs.cardId) (GainKeyword Surge)
      pure o
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      dealDamage attrs eid 2
      doStep 1 msg
      pure o
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      characters <- exhaustableCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
      let card = MkEncounterCard cid (toCardCode attrs)
      chooseOneM pid do
        questionLabeled "rangerOfIthilien.shadow"
        causedBy attrs
        targets characters \character -> do
          exhaust character
          removeShadowCard eid cid
          -- The attacker may have been destroyed by the preceding damage,
          -- sending its shadow cards to the encounter discard pile.
          removeFromEncounterDiscard card
          putObjectiveAllyIntoPlayUnder pid card
        labeled "rangerOfIthilien.decline" $ pure ()
      pure o
    _ -> RangerOfIthilien <$> runMessage msg attrs
