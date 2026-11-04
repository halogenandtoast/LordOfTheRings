module LordOfTheRings.Ally.Cards.Starla (starla) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Helpers.Campaign (campaignLevel)

newtype Starla = Starla AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A scalable boon of the Dream-chaser campaign: a secret of Temple of the Deceived, then
-- in its player's deck.
-- Response: After Starla is discarded from your hand or deck, play her.
starla :: AllyCard Starla
starla = ally (\a -> Starla a {allyDiscardWindow = Just $ discarded a}) Cards.starla
 where
  discarded a =
    AnyWindowOf
      [ CardDiscardedFromHandWindow After a.owner a.allyCardId
      , DiscardedFromDeckWindow After a.owner (Just a.allyCardId)
      ]

-- | X is half the level of Starla, rounded up: her willpower, attack and defense.
instance HasModifiersFor Starla where
  getModifiersFor (Starla a) = do
    x <- (\l -> (l + 1) `div` 2) <$> campaignLevel a
    modifySelf a [WillpowerModifier x, AttackModifier x, DefenseModifier x]

-- | Response: After Starla exhausts, place 1 progress on a location.
instance HasAbilities Starla where
  getAbilities (Starla a) = [restricted a 1 (exists LocationNotImmuneToPlayerCards) $ response $ ExhaustedWindow After (toTarget a)]

instance RunMessage Starla where
  runMessage msg s@(Starla a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- select LocationNotImmuneToPlayerCards
      chooseOrRunOneM pid do
        questionLabeled "starla.progress"
        causedBy a
        targets locations \l -> placeProgress a l 1
      pure s
    -- played from the discard pile, paying her cost
    RespondFromDiscard pid card windows | card.id == a.allyCardId -> do
      removeFromDiscard pid card.id
      push $ PayForCard pid (PlayerCard card) False windows
      pure s
    _ -> Starla <$> runMessage msg a
