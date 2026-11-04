module LordOfTheRings.Event.Cards.FlameOfAnor (flameOfAnor) where

import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Istari))

newtype FlameOfAnor = FlameOfAnor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

flameOfAnor :: EventCard FlameOfAnor
flameOfAnor =
  eventWith FlameOfAnor Cards.flameOfAnor \a ->
    a
      & criteriaL
      .~ ( CharactersAtLeast 1 (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Istari) (AllyControlledBy (PlayerWithId a.owner) <> AllyWithTrait Istari)
            <> PlayerExists (PlayerWithId a.owner <> PlayerWithCardsInDeck 1)
         )

-- | Action: Add Flame of Anor to the victory display and discard the top card of your deck
-- to ready an Istari character you control. That character gets +X [attack] until the end
-- of the phase where X is the discarded card's cost.
instance RunMessage FlameOfAnor where
  runMessage msg e@(FlameOfAnor attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      addToVictoryDisplay card
      top <- headMay . (.deck) <$> getPlayer pid
      discardTopOfDeck pid 1
      let x = case cdCost . toCardDef <$> top of
            Just (Just (StaticCost n)) -> n
            _ -> 0
      let you = PlayerWithId pid
      istari <- selectCharacters (HeroControlledBy you <> HeroWithTrait Istari) (AllyControlledBy you <> AllyWithTrait Istari)
      chooseOrRunOneM pid do
        questionLabeled "flameOfAnor.choose"
        causedBy attrs
        targets istari \c -> do
          ready c
          when (x > 0) $ phaseModifier attrs c (AttackModifier x)
      pure e
    _ -> FlameOfAnor <$> runMessage msg attrs
