module LordOfTheRings.Location.Cards.WestGate (westGate) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers
import LordOfTheRings.Trait (Trait (Osgiliath))

newtype WestGate = WestGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

westGate :: LocationCard WestGate
westGate = location WestGate Cards.westGate

-- | Action: If there is no active location, search the encounter deck and discard pile for
-- an Osgiliath location, reveal it and add it to the staging area. Then, make West Gate the
-- active location. Shuffle the encounter deck.
-- Offered under a player's control too, which then loses it as it becomes active (ruling:
-- a controlled Osgiliath location may still be made active).
-- Forced: After the player who controls West Gate lets an attack go undefended, return
-- West Gate to the staging area.
instance HasAbilities WestGate where
  getAbilities (WestGate a) =
    [anyPlayer $ restricted a 1 (NotCriterion $ exists IsActiveLocation) $ ActionAbility Free]
      <> returnsWhenUndefended a 2

instance RunMessage WestGate where
  runMessage msg l@(WestGate a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      pile <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      let osgiliath = filter (cardMatches $ CardWithTrait Osgiliath <> CardWithType LocationType)
      chooseOneM pid do
        questionLabeled "westGate.search"
        causedBy a
        showing $ map EncounterCard (deck <> pile)
        targets (osgiliath deck) \c -> do
          removeFromEncounterDeck c
          push $ RevealCard c
        targets (osgiliath pile) revealFromEncounterDiscard
      makeActive a.id
      shuffleEncounterDeck
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      returnToStaging a.id
      pure l
    _ -> WestGate <$> runMessage msg a
