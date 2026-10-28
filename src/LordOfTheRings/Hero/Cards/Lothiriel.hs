module LordOfTheRings.Hero.Cards.Lothiriel (lothiriel) where

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
    [ restricted
        a
        1
        (CardInHandMatching a.owner $ CardWithType #ally <> CardSharesTraitWithHero (HeroWithId a.id))
        $ response
        $ CommittedToQuestWindow After (toTarget a)
    ]

instance RunMessage Lothiriel where
  runMessage msg h@(Lothiriel attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      player <- getPlayer pid
      matching <-
        filterM
          (matchCard $ CardWithType #ally <> CardSharesTraitWithHero (HeroWithId attrs.id))
          player.hand
      allies <- filterM passesUnique matching
      chooseOneM pid do
        questionLabeled "lothiriel.chooseAlly"
        causedBy attrs
        targets allies \c -> do
          putIntoPlayCommitted pid c.id
          phaseModifier attrs (CardIdTarget c.id) ShuffleIntoDeckAtEndOfPhase
      pure h
    _ -> Lothiriel <$> runMessage msg attrs
