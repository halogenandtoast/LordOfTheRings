module LordOfTheRings.Hero.Cards.Bifur (bifur) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Bifur = Bifur HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bifur :: HeroCard Bifur
bifur = hero Bifur Cards.bifur

-- | Action: Pay 1 resource from a hero's resource pool to add 1 resource to Bifur's
-- resource pool. Any player may trigger this ability. (Limit once per round.)
--
-- ponytail: only Bifur's controller is offered it; any player may in multiplayer
instance HasAbilities Bifur where
  getAbilities (Bifur a) =
    [ limited (GroupLimit PerRound 1)
        $ restricted a 1 (exists $ HeroWithResources <> not_ (HeroWithId a.heroId)) (ActionAbility Free)
    ]

instance RunMessage Bifur where
  runMessage msg h@(Bifur attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      payers <- select (HeroWithResources <> not_ (HeroWithId attrs.heroId))
      chooseOrRunOneM pid do
        questionLabeled "bifur.pay"
        causedBy attrs
        targets payers \payer -> do
          removeTokens attrs payer Resource 1
          placeResources attrs attrs 1
      pure h
    _ -> Bifur <$> runMessage msg attrs
