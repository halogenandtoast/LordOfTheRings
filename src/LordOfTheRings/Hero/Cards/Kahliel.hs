module LordOfTheRings.Hero.Cards.Kahliel (kahliel) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (Trait (Harad))

newtype Kahliel = Kahliel HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

kahliel :: HeroCard Kahliel
kahliel = hero Kahliel Cards.kahliel

haradAlly :: CardMatcher
haradAlly = CardWithType AllyType <> CardWithTrait Harad

-- | You may use resources from Kahliel's resource pool to pay for Harad ally cards of any
-- sphere.
instance HasModifiersFor Kahliel where
  getModifiersFor (Kahliel a) = modifySelf a [GainResourceIconFor haradAlly s | s <- [Leadership, Tactics, Spirit, Lore]]

-- | Action: Discard a Harad ally card from your hand to ready a Harad character in play.
-- (Limit once per phase.)
instance HasAbilities Kahliel where
  getAbilities (Kahliel a) =
    [ limited (GroupLimit PerPhase 1)
        $ restricted a 1 (CardInHandMatching a.owner haradAlly <> CharactersAtLeast 1 (ExhaustedHero <> HeroWithTrait Harad) (ExhaustedAlly <> AllyWithTrait Harad))
        $ ActionAbility Free
    ]

instance RunMessage Kahliel where
  runMessage msg h@(Kahliel attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      hand <- (.hand) <$> getPlayer pid
      let allies = filter (cardMatches haradAlly) hand
      exhausted <- selectCharacters (ExhaustedHero <> HeroWithTrait Harad) (ExhaustedAlly <> AllyWithTrait Harad)
      chooseOrRunOneM pid do
        questionLabeled "chooseCardToDiscardFromHand"
        causedBy attrs
        targets allies \card -> do
          discardFromHand pid card
          chooseOrRunOneM pid do
            questionLabeled "chooseCharacter"
            causedBy attrs
            targets exhausted ready
      pure h
    _ -> Kahliel <$> runMessage msg attrs
