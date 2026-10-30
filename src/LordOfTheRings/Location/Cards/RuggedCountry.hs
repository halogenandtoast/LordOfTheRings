module LordOfTheRings.Location.Cards.RuggedCountry (ruggedCountry) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RuggedCountry = RuggedCountry LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruggedCountry :: LocationCard RuggedCountry
ruggedCountry = location RuggedCountry Cards.ruggedCountry

-- | If the current quest is a side quest, Rugged Country gets +2 threat.
instance HasModifiersFor RuggedCountry where
  getModifiersFor (RuggedCountry a) = do
    side <- currentQuestIsSideQuest
    when side $ modifySelf a [ThreatModifier 2]

-- | Shadow: Attacking enemy gets +1 attack for each quest card in play.
instance RunMessage RuggedCountry where
  runMessage msg l@(RuggedCountry a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid =<< questCardsInPlay
      pure l
    _ -> RuggedCountry <$> runMessage msg a
