module LordOfTheRings.Quest.Cards.TheCapeOfAndrast (theCapeOfAndrast) where

import LordOfTheRings.Card (CardType (TreacheryType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Weather))

newtype TheCapeOfAndrast = TheCapeOfAndrast QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCapeOfAndrast :: QuestCard TheCapeOfAndrast
theCapeOfAndrast = quest TheCapeOfAndrast Cards.theCapeOfAndrast

-- | Sailing. Forced: After a Weather treachery is revealed, deal 1 damage to each Ship
-- objective and Ship enemy at this stage (one in the Stormcaller's area is at its stage).
-- If you defeat this stage, you have caught up to the Stormcaller and win the game: it is
-- the last stage.
instance HasAbilities TheCapeOfAndrast where
  getAbilities (TheCapeOfAndrast a) = [mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Weather | a.flipped]

-- | When Revealed: Discard cards from the top of the encounter deck until a Weather
-- treachery is discarded. Resolve that card's "when revealed" effect. That effect cannot
-- be canceled.
instance RunMessage TheCapeOfAndrast where
  runMessage msg q@(TheCapeOfAndrast attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheCapeOfAndrast <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      discardTopOfEncounterDeck attrs $ maybe (length deck) (+ 1) (findIndex isWeather deck)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ resolveWhenRevealedFromDiscard (take 1 $ filter isWeather cards)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ships <- select shipObjective
      for_ ships \s -> dealDamage attrs (AllyTarget s) 1
      enemies <- select (shipEnemy <> not_ EnemyInStormcallerArea)
      for_ enemies \e -> dealDamage attrs e 1
      pure q
    _ -> TheCapeOfAndrast <$> runMessage msg attrs
   where
    isWeather c = cdCardType (toCardDef c) == TreacheryType && Weather `member` cdCardTraits (toCardDef c)
