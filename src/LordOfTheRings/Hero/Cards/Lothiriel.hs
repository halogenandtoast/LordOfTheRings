module LordOfTheRings.Hero.Cards.Lothiriel (lothiriel) where

import LordOfTheRings.Card (CardType (AllyType), cdCardTraits, toCardDef)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype Lothiriel = Lothiriel HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

lothiriel :: HeroCard Lothiriel
lothiriel = hero Lothiriel Cards.lothiriel

instance HasModifiersFor Lothiriel where
  getModifiersFor (Lothiriel a) = do
    eomer <- selectAny (CharacterWithTitle "Éomer")
    when eomer $ modifySelf a [GainTrait Rohan]

instance HasAbilities Lothiriel where
  getAbilities (Lothiriel a) =
    [ restricted a 1 (CardInHand a.owner #ally) $ response $ CommittedToQuestWindow After (toTarget a)
    ]

instance RunMessage Lothiriel where
  runMessage msg h@(Lothiriel attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      player <- getPlayer pid
      gained <- mapMaybe (\case GainTrait t -> Just t; _ -> Nothing) <$> getModifiers attrs
      let hers = cdCardTraits (toCardDef attrs) <> setFromList gained
          shares c = not $ null $ intersect hers (cdCardTraits $ toCardDef c)
      allies <- filterM passesUnique [c | c <- player.hand, c.cardType == AllyType]
      chooseOneM pid do
        questionLabeled "lothiriel.chooseAlly"
        causedBy attrs
        targets allies \c -> when (shares c) do
          push $ PutAllyIntoPlay pid c.id
          push $ CommitToQuest (CardIdTarget c.id)
          phaseModifier attrs (CardIdTarget c.id) ShuffleIntoDeckAtEndOfPhase
      pure h
    _ -> Lothiriel <$> runMessage msg attrs
