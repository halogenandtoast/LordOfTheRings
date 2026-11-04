module LordOfTheRings.Enemy.Cards.ThaneUlchorWrathAndRuin (thaneUlchorWrathAndRuin) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ThaneUlchorWrathAndRuin = ThaneUlchorWrathAndRuin EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Wrath and Ruin's Thane Ulchor (22011).
thaneUlchorWrathAndRuin :: EnemyCard ThaneUlchorWrathAndRuin
thaneUlchorWrathAndRuin = enemy ThaneUlchorWrathAndRuin Cards.thaneUlchorWrathAndRuin

-- | Immune to player card effects. Cannot leave the staging area.
instance HasModifiersFor ThaneUlchorWrathAndRuin where
  getModifiersFor (ThaneUlchorWrathAndRuin a) =
    modifySelf a [ImmuneToPlayerCardEffects, CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: After the active location is explored, the first player must choose: either
-- discard cards from the top of the encounter deck until a location is discarded and add
-- it to the staging area, or Thane Ulchor attacks you.
instance HasAbilities ThaneUlchorWrathAndRuin where
  getAbilities (ThaneUlchorWrathAndRuin a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow After]

instance RunMessage ThaneUlchorWrathAndRuin where
  runMessage msg e@(ThaneUlchorWrathAndRuin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "thaneUlchorWrathAndRuin.choose"
        causedBy attrs
        labeled "thaneUlchorWrathAndRuin.location" $ discardEncounterCardsUntil attrs locationCard
        labeled "thaneUlchorWrathAndRuin.attack" $ immediateAttack lead attrs.id
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ find (cardMatches locationCard) cards
      pure e
    _ -> ThaneUlchorWrathAndRuin <$> runMessage msg attrs
   where
    locationCard = CardWithType LocationType
