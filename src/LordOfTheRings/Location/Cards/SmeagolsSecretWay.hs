module LordOfTheRings.Location.Cards.SmeagolsSecretWay (smeagolsSecretWay) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (discardRandomFromEachHand)
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (readySmeagol)

newtype SmeagolsSecretWay = SmeagolsSecretWay LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Added to the staging area by stage 1A.
smeagolsSecretWay :: LocationCard SmeagolsSecretWay
smeagolsSecretWay = location SmeagolsSecretWay Cards.smeagolsSecretWay

-- | Immune to player card effects.
instance HasModifiersFor SmeagolsSecretWay where
  getModifiersFor (SmeagolsSecretWay a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After Sméagol's Secret Way is explored, each player either exhausts a character
-- he controls, or discards 1 random card from his hand.
instance HasAbilities SmeagolsSecretWay where
  getAbilities (SmeagolsSecretWay a) = [mkAbility a 1 $ forced $ ExploredWindow After a.id]

-- | Travel: Either exhaust Sméagol, or discard 1 random card from each player's hand. The
-- first player chooses; with Sméagol exhausted, the cards are discarded.
instance RunMessage SmeagolsSecretWay where
  runMessage msg l@(SmeagolsSecretWay a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      smeagols <- select readySmeagol
      chooseOrRunOneM lead do
        questionLabeled "smeagolsSecretWay.travel"
        causedBy a
        unless (null smeagols) $ labeled "smeagolsSecretWay.exhaustSmeagol" $ traverse_ exhaust smeagols
        labeled "smeagolsSecretWay.discard" $ discardRandomFromEachHand 1
      SmeagolsSecretWay <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        characters <- (<>) <$> (map toTarget <$> select (HeroControlledBy (PlayerWithId p) <> ReadyHero)) <*> (map toTarget <$> select (AllyControlledBy (PlayerWithId p) <> ReadyAlly))
        hand <- (.hand) <$> getPlayer p
        chooseOrRunOneM p do
          questionLabeled "smeagolsSecretWay.explored"
          causedBy a
          targets characters exhaust
          unless (null hand && notNull characters) $ labeled "smeagolsSecretWay.discardOne" $ discardRandomFromHand p 1
      pure l
    _ -> SmeagolsSecretWay <$> runMessage msg a
