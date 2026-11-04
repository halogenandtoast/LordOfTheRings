module LordOfTheRings.Hero.Cards.Thurindir (thurindir) where

import LordOfTheRings.Card (CardType (PlayerSideQuestType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Thurindir = Thurindir HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

thurindir :: HeroCard Thurindir
thurindir = hero Thurindir Cards.thurindir

-- | Thurindir gets +1 willpower for each side quest in the victory display.
instance HasModifiersFor Thurindir where
  getModifiersFor (Thurindir a) = do
    n <- getSideQuestsInVictoryDisplay
    when (n > 0) $ modifySelf a [WillpowerModifier n]

instance RunMessage Thurindir where
  runMessage msg (Thurindir attrs) = case msg of
    -- Setup: Search your deck for a side quest and add it to your hand. Shuffle your deck.
    -- Before the setup hand is drawn (step 5), and kept through a mulligan: Caleb's ruling,
    -- as reported on the FFG forum ("Thurindir and Initial Hand", "Setup Abilities on Hero
    -- Cards"); a later report puts it after the scenario's setup instead, unconfirmed.
    DoStep 3 Setup -> do
      searchDeck attrs.owner attrs maxBound (CardWithType PlayerSideQuestType) False
      Thurindir <$> runMessage msg attrs
    _ -> Thurindir <$> runMessage msg attrs
