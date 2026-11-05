module LordOfTheRings.Location.Cards.BanksOfMorgulduin (banksOfMorgulduin) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Captain))

newtype BanksOfMorgulduin = BanksOfMorgulduin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

banksOfMorgulduin :: LocationCard BanksOfMorgulduin
banksOfMorgulduin = location BanksOfMorgulduin Cards.banksOfMorgulduin

-- | Forced: When Banks of Morgulduin is explored, deal it facedown as a shadow card to a
-- Captain enemy. The first player picks among several; with none it is discarded as usual.
instance HasAbilities BanksOfMorgulduin where
  getAbilities (BanksOfMorgulduin a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Shadow: If attacking enemy is a Captain, deal it 3 additional shadow cards for this
-- attack.
instance RunMessage BanksOfMorgulduin where
  runMessage msg l@(BanksOfMorgulduin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      captains <- select (EnemyWithTrait Captain)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "banksOfMorgulduin.captain"
        causedBy a
        targets captains \eid -> do
          let card = MkEncounterCard a.cardId (toCardCode a)
          cancelMatching \case
            Discard _ (LocationTarget lid) -> lid == a.id
            _ -> False
          discard a a
          removeFromEncounterDiscard card
          whileInPlay (EnemyTarget eid) $ push $ AttachShadowCard eid card
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      captain <- selectAny (EnemyWithId eid <> EnemyWithTrait Captain)
      when captain $ dealShadowCardsDuringAttack eid pid defenders 3
      pure l
    _ -> BanksOfMorgulduin <$> runMessage msg a
