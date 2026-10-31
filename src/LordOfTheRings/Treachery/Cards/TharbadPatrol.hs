module LordOfTheRings.Treachery.Cards.TharbadPatrol (tharbadPatrol) where

import LordOfTheRings.Card (CardDef (cdCardType), toCardCode, toCardDef)
import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TharbadPatrol = TharbadPatrol TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tharbadPatrol :: TreacheryCard TharbadPatrol
tharbadPatrol = treachery TharbadPatrol Cards.tharbadPatrol

-- | Counts as a Condition attachment with the text: "Forced: After the players travel to
-- attached location, each player must discard the top 2 cards from the encounter deck and
-- engage each enemy he discarded in this way."
instance HasAbilities TharbadPatrol where
  getAbilities (TharbadPatrol a) = case a.placement of
    AttachedTo (LocationTarget lid) -> [mkAbility a 1 $ forced $ TraveledWindow After lid]
    _ -> []

-- | Surge.
-- When Revealed: Attach Tharbad Patrol to a non-unique location in the staging area
-- without a copy of Tharbad Patrol attached.
instance RunMessage TharbadPatrol where
  runMessage msg t@(TharbadPatrol attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      patrolled <- getsGame (map (.placement) . filter ((== toCardCode attrs) . toCardCode) . toList . (.entities.treacheries))
      locations <- filter ((`notElem` patrolled) . AttachedTo . toTarget) <$> select (LocationInStagingArea <> NotLocation UniqueLocation)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "tharbadPatrol.attach"
        causedBy attrs
        targets locations (attachTreachery attrs.treacheryId)
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> discardTopOfEncounterDeckFor attrs (PlayerTarget p) 2
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget pid) cards -> do
      for_ (filter ((== EnemyType) . cdCardType . toCardDef) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged pid card
      pure t
    _ -> TharbadPatrol <$> runMessage msg attrs
