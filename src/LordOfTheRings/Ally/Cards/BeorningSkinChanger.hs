module LordOfTheRings.Ally.Cards.BeorningSkinChanger (beorningSkinChanger) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (AllyType))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Beorning))

newtype BeorningSkinChanger = BeorningSkinChanger AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

beorningSkinChanger :: AllyCard BeorningSkinChanger
beorningSkinChanger = ally BeorningSkinChanger Cards.beorningSkinChanger

bigBeorning :: CardMatcher
bigBeorning = CardWithType AllyType <> CardWithTrait Beorning <> CardWithPrintedCostAtLeast 3

-- | Combat Action: Choose a Beorning ally with printed cost 3 or more in your hand or
-- discard pile. Discard Beorning Skin-changer to put that ally into play.
instance HasAbilities BeorningSkinChanger where
  getAbilities (BeorningSkinChanger a) =
    [ restricted a 1 (DuringPhase CombatPhase <> AnyCriteria [CardInHandMatching pid bigBeorning, CardInDiscard pid bigBeorning])
        $ ActionAbility Free
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage BeorningSkinChanger where
  runMessage msg s@(BeorningSkinChanger a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      player <- getPlayer pid
      let mine = filter (cardMatches bigBeorning)
      fromHand <- filterM passesUnique (mine player.hand)
      fromDiscard <- filterM passesUnique (mine player.discard)
      chooseOneM pid do
        questionLabeled "beorningSkinChanger.ally"
        causedBy a
        targets fromHand \c -> do
          discard a
          putAllyIntoPlayFromHand pid c.id
        targets [pc | PlayerCard pc <- fromDiscard] \pc -> do
          discard a
          removeFromDiscard pid pc.id
          putAllyIntoPlayUnder pid pc
      pure s
    _ -> BeorningSkinChanger <$> runMessage msg a
