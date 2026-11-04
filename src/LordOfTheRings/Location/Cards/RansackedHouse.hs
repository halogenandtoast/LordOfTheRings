module LordOfTheRings.Location.Cards.RansackedHouse (ransackedHouse) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Helpers.ControlledLocation (losesControlOfAnother, returnToStaging, takeControlOfLocation)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (InPlayArea))

newtype RansackedHouse = RansackedHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ransackedHouse :: LocationCard RansackedHouse
ransackedHouse = location RansackedHouse Cards.ransackedHouse

-- | Encounter Action: Put the topmost enemy in the encounter discard pile into play engaged
-- with you to take control of Ransacked House. Any player may, but not one who controls it.
instance HasAbilities RansackedHouse where
  getAbilities (RansackedHouse a) =
    [ anyPlayer
        $ restricted
          a
          1
          ( DuringPhase EncounterPhase
              <> CardInEncounterDiscard (CardWithType EnemyType)
              <> NotCriterion (LocationExists $ LocationWithId a.id <> LocationControlledBy ActivePlayer)
          )
        $ ActionAbility Free
    ]

-- | While you control Ransacked House, it gains: "Forced: After the player who controls
-- Ransacked House loses control of a location, return Ransacked House to the staging area."
instance RunMessage RansackedHouse where
  runMessage msg l@(RansackedHouse a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discards <- getEncounterDiscard
      for_ (find (cardMatches $ CardWithType EnemyType) discards) \enemy -> do
        removeFromEncounterDiscard enemy
        putEnemyIntoPlayEngaged pid enemy
        takeControlOfLocation pid a.id
      pure l
    _ | InPlayArea _ <- a.placement -> do
      whenM (losesControlOfAnother a msg) $ returnToStaging a.id
      RansackedHouse <$> runMessage msg a
    _ -> RansackedHouse <$> runMessage msg a
