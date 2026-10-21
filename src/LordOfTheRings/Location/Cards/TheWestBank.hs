module LordOfTheRings.Location.Cards.TheWestBank (theWestBank) where

import LordOfTheRings.Card (CardDef (..), CardType (AttachmentType, EventType), toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Clue))

newtype TheWestBank = TheWestBank LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWestBank :: LocationCard TheWestBank
theWestBank = location TheWestBank Cards.theWestBank

-- | While The West Bank is the active location, attachment and event cards cost 1
-- additional matching resource to play from hand.
instance HasModifiersFor TheWestBank where
  getModifiersFor (TheWestBank a) =
    when (a.placement == ActiveLocation)
      $ modified a GameTarget [IncreaseCost (CardWithType AttachmentType) 1, IncreaseCost (CardWithType EventType) 1]

-- | Shadow: If you do not control at least 1 hero with a Clue card attached, double this
-- enemy's base attack for this attack.
instance RunMessage TheWestBank where
  runMessage msg l@(TheWestBank a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      clues <- selectAny (HeroControlledBy (PlayerWithId pid) <> HeroWithAttached Clue)
      base <- fromMaybe 0 . cdAttack . toCardDef <$> getEnemy eid
      unless clues $ enemyGetsAttack a eid base
      pure l
    _ -> TheWestBank <$> runMessage msg a
