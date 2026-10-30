module LordOfTheRings.Ally.Cards.Iarion (iarion) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))

newtype Iarion = Iarion AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
iarion :: AllyCard Iarion
iarion = ally Iarion Cards.iarion

-- | X is the number of quest cards in play.
instance HasModifiersFor Iarion where
  getModifiersFor (Iarion a) = do
    x <- questCardsInPlay
    modifySelf a [WillpowerModifier x, AttackModifier x, DefenseModifier x]

-- | Response: After a side quest is revealed from the encounter deck, ready Iârion.
-- If Iârion leaves play, the players lose the game: read off the window he leaves play in,
-- however he leaves.
instance HasAbilities Iarion where
  getAbilities (Iarion a) =
    [ mkAbility a 1 $ response $ SideQuestRevealedWindow After
    , mkAbility a 2 $ forced $ LeftPlayWindow After (toTarget a)
    ]

instance RunMessage Iarion where
  runMessage msg s@(Iarion a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure s
    _ -> Iarion <$> runMessage msg a
