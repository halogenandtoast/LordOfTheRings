module LordOfTheRings.Hero.Cards.GaldorOfTheHavensHero (galdorOfTheHavensHero) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype GaldorOfTheHavensHero = GaldorOfTheHavensHero HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

galdorOfTheHavensHero :: HeroCard GaldorOfTheHavensHero
galdorOfTheHavensHero = hero GaldorOfTheHavensHero Cards.galdorOfTheHavensHero

-- | Response: After drawing your setup hand, instead of taking a mulligan, you may discard
-- any number of cards from your hand. Then, draw that many cards.
-- Action: If you have no cards in your hand, draw 6 cards. (Limit once per game.)
instance HasAbilities GaldorOfTheHavensHero where
  getAbilities (GaldorOfTheHavensHero a) =
    [ mkAbility a 1 $ response $ SetupHandDrawnWindow After a.owner
    , limited (PlayerLimit PerGame 1)
        $ restricted a 2 (PlayerExists $ PlayerWithId a.owner <> NotPlayer (PlayerWithCardsInHand 1))
        $ ActionAbility Free
    ]

instance RunMessage GaldorOfTheHavensHero where
  runMessage msg h@(GaldorOfTheHavensHero attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      skipMulligan pid
      mayDiscardFromHand attrs pid
      pure h
    EachDiscardAnyFromHand (isSource attrs -> True) [] discarded -> do
      drawCards attrs attrs.owner (length discarded)
      pure h
    UseCardAbility pid (isSource attrs -> True) 2 _ -> do
      drawCards attrs pid 6
      pure h
    _ -> GaldorOfTheHavensHero <$> runMessage msg attrs
